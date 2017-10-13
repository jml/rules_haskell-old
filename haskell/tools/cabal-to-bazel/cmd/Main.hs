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
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName(..), packageNameToUnqualComponentName)

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

data BazelRule = BazelRule

printBazelRule :: BazelRule -> String
printBazelRule _ = "hahahaha\n"

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
makeBazelRules _ _ = [BazelRule]

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
            print (map printBazelRule (packageDescriptionToBazel packageDescription))
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
