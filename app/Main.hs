module Main (main) where

import Lexer as L 

main :: IO ()
main = print $ L.alexScanTokens "."


