module Main (main) where

import qualified ModuleOne.Foo as Foo1
import qualified ModuleTwo.Foo as Foo2

main :: IO ()
main = do
  putStrLn $ "foo1: " ++ show Foo1.foo
  putStrLn $ "foo2: " ++ show Foo2.foo
