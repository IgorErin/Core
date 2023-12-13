module PSeq (T(), nil, str, append, nl, indent, display, merge, interleav) where 

data T = Nil | Str String | Append T T 

nil :: T 
nil = Nil 

str :: String -> T 
str = Str 

append :: T -> T -> T 
append = Append 

nl :: T 
nl = Str "\n"

indent :: T -> T 
indent = id 

merge :: [T] -> T 
merge  = foldl append nil   

interleav :: T -> [T] -> T 
interleav sep ls = 
    let add f s = merge [f, sep, s] in 
    foldl add nil ls 

display :: T -> String 
display = undefined

flatten :: [T] -> String
flatten [] = ""
flatten (Nil : tl) = flatten tl 
flatten (Str s : tl ) = s ++ flatten tl
flatten (Append f s : tl) = flatten (f : s : tl) 
