module Assoc where

import qualified Data.List as L
import qualified Data.Maybe as Mb

type T k v = [(k, v)] 

lookup :: Eq t => t -> T t a -> Maybe a
lookup _ [] = Nothing 
lookup k (~(someKey, value) : tl) 
    | k == someKey = Just value 
    | otherwise    = Assoc.lookup k tl 

lookupWith :: Eq t => t -> T t a -> a -> a 
lookupWith key ls def = Mb.fromMaybe def $ Assoc.lookup key ls   

delete :: Eq t => t -> T t a -> T t a
delete _ [] = []
delete k (~item@(key, _) : tl) 
    | k == key = delete k tl 
    | otherwise = item : delete k tl  

countKey :: Eq t => t -> T t a -> Int
countKey someKey ls = sum bitmap 
    where 
        bitmap = map (\ ~(key, _) -> if someKey == key then 1 else 0) ls    

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

init :: T a b 
init = []

insert :: T a b -> a -> b -> T a b 
insert ls key value = (key, value) : ls