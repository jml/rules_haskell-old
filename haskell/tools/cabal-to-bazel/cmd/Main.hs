module Main (main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (makeAbsolute)
import System.FilePath ((</>), normalise, makeRelative, takeDirectory)
import System.IO (hPutStrLn, stderr)

import Distribution.Compiler (CompilerInfo)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
  ( BuildInfo(..)
  , GenericPackageDescription(..)
  , Library(..)
  , PackageDescription(..)
  )
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parse (ParseResult(..), parseGenericPackageDescription)
import qualified Distribution.Simple.GHC as CabalGHC
import Distribution.Simple.Compiler (compilerInfo)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.System (buildPlatform)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.PackageName (PackageName)

import qualified Digraph
import qualified GHC
import qualified GHC.Paths
import HscTypes (msHsFilePath)

-- Questions
--
-- - what is a manual flag?
-- - what are subLibraries?
-- - what should I use instead of buildDepends?
-- - what are extraSrcFiles, extraTmpFiles, and extraDocFiles?

-- `BuildInfo` seems very much what I want

-- Looks like Cabal turns a GenericPackageDescription into a specific PackageDescription
-- We need to do something like that, but for all possible flags (?)

-- TODO: Try out ppPackageDescription

-- | A Bazel label.
--
-- See https://docs.bazel.build/versions/master/build-ref.html#labels
type Label = String

-- | A set of Bazel labels.
type Labels = [Label]

-- | The name of a target within a package.
--
-- See https://docs.bazel.build/versions/master/build-ref.html#name
type TargetName = String

-- | Attributes that are common across Haskell Bazel rules.
data HaskellAttributes
  = HaskellAttributes
  { -- | Sources for this Bazel target.
    srcs :: !Labels
    -- | Dependencies for this Bazel target.
  , deps :: !Labels
    -- | Data dependencies for the Bazel target.
  , dataDeps :: !Labels
    -- | The root of the module hierarchy.
  , srcDir :: FilePath
    -- | GHC packages that we depend on.
  , packages :: [PackageName]
  } deriving (Show)

-- | A single Bazel rule for building Haskell.
data BazelRule
  = HsLibrary TargetName HaskellAttributes
  | HsBinary TargetName HaskellAttributes FilePath
  | HsTest TargetName HaskellAttributes FilePath
  deriving (Show)

printBazelRule :: BazelRule -> String
printBazelRule (HsLibrary name attrs) =
  printRule "hs_library" $ printName name ++ printAttrs attrs
printBazelRule (HsBinary name attrs mainIs) =
  printRule "hs_binary" $ printName name ++ printAttrs attrs ++ printMainIs mainIs
printBazelRule (HsTest name attrs mainIs) =
  printRule "hs_test" $ printName name ++ printAttrs attrs ++ printMainIs mainIs

printRule :: String -> [String] -> String
printRule kind elems = unlines $ [ kind ++ "(" ] ++ elems ++ [ ")" ]

printName :: TargetName -> [String]
printName name = ["    name = " ++ show name ++ ","]

printAttrs :: HaskellAttributes -> [String]
printAttrs attrs =
  [ "    srcs = " ++ show (srcs attrs) ++ ","
  , "    deps = " ++ show (deps attrs) ++ ","
  , "    data = " ++ show (dataDeps attrs) ++ ","
  , "    packages = " ++ show (packages attrs) ++ ","
  , "    src_dir = " ++ show (srcDir attrs) ++ ","
  ]

printMainIs :: FilePath -> [String]
printMainIs mainIs = ["    main_is = " ++ show mainIs ++ "," ]

-- | An entire Bazel build file.
newtype BazelBuildFile = BazelBuildFile [BazelRule] deriving (Show)

emptyBuildFile :: BazelBuildFile
emptyBuildFile = BazelBuildFile []

printBuildFile :: BazelBuildFile -> String
printBuildFile (BazelBuildFile rules) = unlines (map printBazelRule rules)

getSrcDir :: FilePath -> Library -> FilePath
getSrcDir cabalDir lib =
  case map toAbsPath $ hsSourceDirs . libBuildInfo $ lib of
    [srcDir'] -> srcDir'
    [] -> cabalDir
    _ -> error "Do not support multiple source directories"
  where
    toAbsPath relDir = normalise $ cabalDir </> relDir

loadDepGraph :: GHC.GhcMonad m => Library -> m GHC.ModuleGraph
loadDepGraph lib = do
  let exposedMods = map (GHC.TargetModule . GHC.mkModuleName . intercalate "." . ModuleName.components) (exposedModules lib)
  let targets = map makeTarget exposedMods
  GHC.setTargets targets
  GHC.depanal [] False -- XXX: MkDepend allows duplicate roots. Do we want that? What does it even mean?
  where
    makeTarget moduleName = GHC.Target { GHC.targetId = moduleName , GHC.targetAllowObjCode = False , GHC.targetContents = Nothing }


ruleForModule :: FilePath -> FilePath -> Digraph.SCC GHC.ModSummary -> BazelRule
ruleForModule _ _ (Digraph.CyclicSCC _nodes) = error "cyclic nodes"
ruleForModule cabalDir srcDir' (Digraph.AcyclicSCC node) =
  HsLibrary (GHC.moduleNameString . GHC.ms_mod_name $ node) attrs
  where
    attrs = HaskellAttributes
            { srcs = [makeRelative cabalDir (msHsFilePath node)]
            , deps = []
            , dataDeps = []
            , srcDir = makeRelative cabalDir srcDir'
            , packages = []
            }

-- TODO: Visibility

-- TODO: Keep a map of modules to rules so we can do dependencies.

packageDescriptionToBazel :: GHC.GhcMonad m => FilePath -> PackageDescription -> m BazelBuildFile
packageDescriptionToBazel cabalFile pkgDesc =
  case library pkgDesc of
    Nothing -> pure emptyBuildFile
    Just lib -> do
      flags <- GHC.getSessionDynFlags
      let srcDir' = getSrcDir cabalDir lib
      _ <- GHC.setSessionDynFlags (flags { GHC.importPaths = [srcDir']
                                         , GHC.ghcMode = GHC.MkDepend
                                         })
      depGraph <- loadDepGraph lib
      let sorted = GHC.topSortModuleGraph False depGraph Nothing
      pure $ BazelBuildFile (map (ruleForModule cabalDir srcDir') sorted)
  where
    cabalDir = takeDirectory cabalFile

loadCompilerInfo :: IO CompilerInfo
loadCompilerInfo = do
  (compiler, _platform, _progDB) <- CabalGHC.configure Verbosity.silent (Just GHC.Paths.ghc) (Just GHC.Paths.ghc_pkg) emptyProgramDb
  pure (compilerInfo compiler)


flattenCabalFile :: GenericPackageDescription -> IO (Either [Dependency] PackageDescription)
flattenCabalFile desc = do
  compilerInfo' <- loadCompilerInfo
  pure $ fst <$> finalizePD flagAssignment componentRequestedSpec depIndex platform compilerInfo' depConstraints desc
  where
    -- No one has explicitly specified flags. In final version, we want to
    -- have a bazel file that somehow supports flags.
    flagAssignment = []
    -- We want to generate everything, so not OneComponentRequestedSpec.
    -- We don't yet have support for running benchmarks.
    componentRequestedSpec = ComponentRequestedSpec { testsRequested = True, benchmarksRequested = False }
    -- Unknown whether a dependency is satisfiable.
    depIndex = const True
    platform = buildPlatform  -- XXX: Apparently should be using LocalBuildInfo.hostPlatform instead.
    -- Do we have any constraints on dependencies? I don't know why we might want these.
    depConstraints = []


convertCabalFile :: FilePath -> IO ()
convertCabalFile cabalFile = do
  contents <- readFile cabalFile
  case parseGenericPackageDescription contents of
    ParseOk [] genericDesc -> do
      desc <- flattenCabalFile genericDesc
      case desc of
        Left missingDeps -> do
          hPutStrLn stderr $ "Missing dependencies: " ++ show missingDeps
          exitFailure
        Right pkgDesc -> do
          absCabalFile <- makeAbsolute cabalFile
          buildFile <- GHC.runGhc (Just GHC.Paths.libdir) (packageDescriptionToBazel absCabalFile pkgDesc)
          putStr (printBuildFile buildFile)
    bad -> do
      hPutStrLn stderr $ "Could not parse file: " ++ show bad
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cabalFile] -> convertCabalFile cabalFile
    _ -> do
      hPutStrLn stderr "Usage: cabal-to-bazel CABALFILE"
      exitFailure
