module Main where

import Foo
import Baz

main :: IO ()
main = do
  putStrLn $ "foo: " ++ show foo
  putStrLn $ "baz: " ++ show baz
