module Main where 

import Test.Tasty
import Tests.Lexer as L (tests) 
import Tests.Parser as P (tests)

allTests :: TestTree
allTests = testGroup "all" [L.tests, P.tests ]

main :: IO ()
main = defaultMain allTests  
