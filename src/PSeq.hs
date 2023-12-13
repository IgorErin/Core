module PSeq (T(), ws, nil, str, append, nl, indent, display, merge, interleav) where 

data T = Nil | Str String | Append T T | Indent T | NewLine 

ws :: T 
ws = Str " "

nil :: T 
nil = Nil 

str :: String -> T 
str = Str 

append :: T -> T -> T 
append = Append 

nl :: T 
nl = NewLine 

indent :: T -> T 
indent = Indent 

merge :: [T] -> T 
merge  = foldl append nil   

interleav :: T -> [T] -> T 
interleav sep ls =
    case ls of 
        hd : tl  ->  
            let add f s = merge [f, sep, s] in 
            foldl add hd tl 
        [] -> Nil 

display :: T -> String 
display = flatten 0  . (: [])

space :: Int -> String
space n  = replicate n " " |> concat 
    where (|>)  x f = f x 

flatten :: Int -> [T] -> String
flatten _      []                         = ""
flatten global (NewLine : tl )       = '\n' : space global ++ flatten global tl 
flatten global ((Indent hd) : tl)    = 
    let newHd = flatten (succ global) [hd] 
        newTl = flatten global tl in
        newHd ++ newTl
flatten global (Nil : tl)            = flatten global tl
flatten global ((Str s) : tl)        = s ++ flatten global tl  
flatten global ((Append l r) : tl)   = mk l ++ mk r ++ flatten global tl
    where 
        mk t = flatten global [t]  