module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Distribution.Compiler (CompilerInfo)
import Distribution.PackageDescription
  ( BuildInfo(..)
  , GenericPackageDescription(..)
  , Library(..)
  , PackageDescription(..)
  , allBuildInfo
  )
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parse (ParseResult(..), parseGenericPackageDescription)
import qualified Distribution.Simple.GHC as GHC
import Distribution.Simple.Compiler (compilerInfo)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.System (buildPlatform)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(..))
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName(..)
  , packageNameToUnqualComponentName
  , unUnqualComponentName
  )

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


packageDescriptionToBazel :: PackageDescription -> [BazelRule]
packageDescriptionToBazel packageDescription =
  case library packageDescription of
    Nothing -> []
    Just lib ->
      let name = getLibraryName (pkgName . package $ packageDescription) lib
      in makeBazelRules name (libBuildInfo lib)

-- | Get the name of the library of this PackageDescription.
getLibraryName :: PackageName -> Library -> UnqualComponentName
getLibraryName packageName lib =
  case libName lib of
    Nothing -> packageNameToUnqualComponentName packageName
    Just name -> name

makeBazelRules :: UnqualComponentName -> BuildInfo -> [BazelRule]
makeBazelRules name _ = [HsLibrary (toTargetName name) (HaskellAttributes [] [] [] [] [])]
  where
    toTargetName = unUnqualComponentName

loadCompilerInfo :: IO CompilerInfo
loadCompilerInfo = do
  (compiler, platform, progDB) <- GHC.configure Verbosity.silent (Just "/usr/local/bin/ghc") (Just "/usr/local/bin/ghc-pkg") emptyProgramDb
  pure (compilerInfo compiler)

convertCabalFile :: FilePath -> IO ()
convertCabalFile cabalFile = do
  contents <- readFile cabalFile
  case parseGenericPackageDescription contents of
    ParseOk [] desc ->
      let
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
      in do
        compilerInfo <- loadCompilerInfo
        case finalizePD flagAssignment componentRequestedSpec depIndex platform compilerInfo depConstraints desc of
          Left missingDeps -> do
            hPutStrLn stderr $ "Missing dependencies: " ++ show missingDeps
            exitFailure
          Right (packageDescription, flagAssignment) -> do
            mapM_ putStrLn (map printBazelRule (packageDescriptionToBazel packageDescription))
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
