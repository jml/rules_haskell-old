module Main where

import Foo
import Bar

main :: IO ()
main = do
  putStrLn $ "foo: " ++ show foo
  putStrLn $ "bar: " ++ show bar
