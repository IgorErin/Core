module Main (main) where
import Frontend as F
import qualified Template as T 
import qualified  Language as L
import qualified Show 

runAndPrint :: String -> String
runAndPrint = Show.strStates . T.eval . T.compile . F.parse 

main :: IO ()
main = putStrLn $ runAndPrint " pair x y f = f x y ; \
\ fst p = p K ; \
\snd p = p K1 ; \
\f x y = letrec \
\a = pair x b ; \
\b = pair y a \
\in \
\fst a ; \
\ main = f 3 4"
