module Main (main) where
import Frontend as F
import qualified Template as T 
import qualified Show 

runAndPrint :: String -> String
runAndPrint = Show.strStates . T.eval . T.compile . F.parse 

main :: IO ()
main = putStrLn $ runAndPrint "id x = x ; main = twice twice twice id 3"