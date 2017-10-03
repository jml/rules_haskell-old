module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Distribution.PackageDescription.Parse (parseGenericPackageDescription)


convertCabalFile :: FilePath -> IO ()
convertCabalFile cabalFile = do
  contents <- readFile cabalFile
  print (parseGenericPackageDescription contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cabalFile] -> convertCabalFile cabalFile
    _ -> do
      hPutStrLn stderr "Usage: cabal-to-bazel CABALFILE"
      exitFailure
  putStrLn "cabal-to-bazel"
