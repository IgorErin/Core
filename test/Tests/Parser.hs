module Tests.Parser (tests) where 

import Lexer as L 
import Parser as P 
import Language as Lang 

import Test.Tasty
import Test.Tasty.HUnit

run :: String -> CoreProgram
run = P.calc . L.alexScanTokens

cases :: [(String, CoreProgram)]
cases = 
    [
        ("f x = x; g x = let y = x in y", [("f",["x"],EVar "x"), ("g",["x"],ELet False [("y",EVar "x")] (EVar "y"))]),
        ("f x = x; main = f 5",[("f",["x"],EVar "x"), ("main",[],EAp (EVar "f") (ENum 5))]),
        ("sum x y = x + y", [("sum",["x","y"],EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))]),
        ("double x = x * 2; double2 x = x + x",[("double",["x"],EAp (EAp (EVar "*") (EVar "x")) (ENum 2)), ("double2",["x"],EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))]),
        ("default x y = case x of <0> -> y ; <1> x -> x", [("default",["x","y"],ECase (EVar "x") [(0,[],EVar "y"), (1,["x"],EVar "x")])]),
        ("square x = x * x ; main = square (square 3)", [("square",["x"],EAp (EAp (EVar "*") (EVar "x")) (EVar "x")), ("main",[],EAp (EVar "square") (EAp (EVar "square") (ENum 3)))])
    ]

tests :: TestTree
tests = 
    testGroup "Parsing" $
    map (\ (str, expected) -> 
            let 
                actual = run  str
            in 
                 testCase str $ actual @?= expected)
        cases 
