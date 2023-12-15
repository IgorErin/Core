module Main where 

import Test.Tasty
import Tests.Lexer as L (tests) 
import Tests.Parser as P (tests)
import Tests.Mark1 as M1 (tests)

allTests :: TestTree
allTests = testGroup "all" [L.tests, P.tests, M1.tests ]

main :: IO ()
main = defaultMain allTests  
