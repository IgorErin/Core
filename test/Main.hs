module Main where 

import Test.Tasty
import Tests.Lexer as L (tests) 
import Tests.Parser as P (tests)
import Tests.Template as T (tests)

allTests :: TestTree
allTests = testGroup "all" [L.tests, P.tests, T.tests ]
 


main :: IO ()
main = defaultMain allTests  
