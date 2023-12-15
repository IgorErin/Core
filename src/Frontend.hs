module Frontend where 

import Lexer as L 
import Parser as P
import Language as Lang 

parse :: String -> Lang.CoreProgram
parse = P.calc . L.alexScanTokens 