module Assoc where

import qualified Data.List as L
type T k v = [(k, v)] 

look :: Eq t => t -> T t a -> Maybe a
look _ [] = Nothing 
look k ((someKey, value) : tl) 
    | k == someKey = Just value 
    | otherwise    = look k tl 

delete :: Eq t => t -> T t a -> T t a
delete _ [] = []
delete k (item@(key, _) : tl) 
    | k == key = delete k tl 
    | otherwise = item : delete k tl  

countKey :: Eq t => t -> T t a -> Int
countKey someKey ls = sum bitmap 
    where 
        bitmap = map (\ (key, _) -> if someKey == key then 1 else 0) ls    

null :: T k v -> Bool
null = L.null

union :: T a b -> T a b -> T a b 
union = (++)

fromList :: [(a, b)] -> T a b 
fromList = id 

update :: Eq a => a -> b -> T a b -> T a b 
update key value ls = 
    (key, value) : cleanLs 
    where cleanLs = delete key ls  