module Greetings where

newtype Greeting = Greeting String

greetingToString :: Greeting -> String
greetingToString (Greeting s) = s
