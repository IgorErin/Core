module Main where 

import Test.HUnit
import Tests.Lexer(tests)

main :: IO Counts
main = runTestTT tests
