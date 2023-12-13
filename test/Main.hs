module Main where 

import Test.Tasty
import Tests.Lexer(tests)

main :: IO ()
main = defaultMain tests
