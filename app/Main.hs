module Main (main) where

import Lexer as L 
import Parser as P 

main :: IO ()
main = print $ P.calc $ L.alexScanTokens "f x = 5"


