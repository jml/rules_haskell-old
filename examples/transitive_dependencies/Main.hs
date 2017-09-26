module Main where

import Foo

main :: IO ()
main = do
  putStrLn ("foo: " ++ show foo)

