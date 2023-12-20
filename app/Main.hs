module Main (main) where
import Frontend as F
import qualified Template as T 
import qualified Show 

runAndPrint :: String -> String
runAndPrint = Show.strStates . T.eval . T.compile . F.parse 

main :: IO ()
main = putStrLn $ runAndPrint "error = error; ceil x = if (x <= 1) 1 ((ceil (x - 1)) * 2); main = ceil 5"