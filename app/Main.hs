module Main (main) where
import Mark1 as M1
import Frontend as F

runAndPrint :: String -> String
runAndPrint = M1.showResult . M1.start . M1.compile . F.parse 

main :: IO ()
main = putStrLn $ runAndPrint "main = (S K I (K I S)) 0"


