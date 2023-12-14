module Tests.Lexer (tests) where 

import Test.Tasty.HUnit
import Test.Tasty
import Lexer as L 

cases :: [(String, [Token])]
cases = 
    [ 
        ("let in case Pack letrec let case of", [L.Let, L.In, L.Case, L.Pack, L.Letrec, L.Let, L.Case, L.Of ]),
        (" -> ", [L.Arrow]),
        ("fun", [L.Lambda]),
        ("+ - / * ", [L.Plus, L.Minus, L.Div, L.Mul]),
        ("== < <= > >=", [L.Eq, L.LT, L.LE, L.GT, L.GE]),
        ("| &", [L.Or, L.And]),
        ("( ) { } < >", [L.LParent, L.RParent, L.LBrace, L.RBrace, L.LT, L.GT ]),
        ("; , .", [L.SemiColon, L.Comma, L.Point]),
        ("=", [L.Assign]),

        ("fun x -> x", [L.Lambda, L.Ident "x", L.Arrow, L.Ident "x"])
    ]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
            let 
                actual = L.alexScanTokens  str
            in 
                 testCase str $ actual @?= expected)
        cases 
    
    