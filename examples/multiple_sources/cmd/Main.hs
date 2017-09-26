module Main (main) where

import CmdSupport
import MyModule.Foo
import MyModule.Bar

main :: IO ()
main = do
  putStrLn $ "cmdSupport: " ++ show cmdSupport
  putStrLn $ "foo: " ++ show foo
  putStrLn $ "bar: " ++ show bar
