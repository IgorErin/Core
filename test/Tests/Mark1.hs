module Tests.Mark1 (tests) where  

import qualified Frontend as F (parse)
import qualified Mark1 as M

import Test.Tasty
import Test.Tasty.HUnit

cases :: [(String, Int)]
cases = 
    [ ("main = S K K 3", 3),
      ("main = (S K I K) 3 0", 3),
      ("main = (S K I (K I S)) 0", 0)]

run :: String -> Int
run = M.run . F.parse 

tests :: TestTree
tests = 
    testGroup "Mark1" $
    map (\ (str, expected) -> 
            let 
                actual = run str
            in 
                 testCase str $ actual @?= expected)
        cases 

