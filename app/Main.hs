module Main (main) where

import Language as L 
import PSeq as P

e :: CoreExpr 
e = 
 ELet  
    False
    [("var", ENum 4), 
    ("second_var", 
        ELet False [("lol" , ENum 4)] $ ENum 4 ), 
    ("lol", ENum 4) ]
    (EVar "var")

main :: IO ()
main = putStrLn $ P.display $ L.pprExpr e 
