module Greeter where

import Greetings (Greeting)

greet :: Greeting -> IO ()
greet greeting = putStrLn (greetingToString greeting)
