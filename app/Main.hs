module Main (main) where
import Frontend as F
import qualified Template as T 
import qualified Show 

runAndPrint :: String -> String
runAndPrint = Show.strStates . T.eval . T.compile . F.parse 

main :: IO ()
main = putStrLn $ runAndPrint "fac n = if (n == 0) 1 (n * fac (n-1)) ; \
             \ main = fac 1"