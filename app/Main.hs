module Main (main) where
-- import Mark1 as M1
import Frontend as F

import Language as L

-- runAndPrint :: String -> String
-- runAndPrint = M1.showResult . M1.start . M1.compile . F.parse 

main :: IO ()
main = print ()
    
--      putStrLn $ runAndPrint " pair x y f = f x y ; \
-- \ fst p = p K ; \
-- \snd p = p K1 ; \
-- \f x y = letrec \
-- \a = pair x b ; \
-- \b = pair y a \
-- \in \
-- \fst a ; \
-- \ main = f 3 4"
