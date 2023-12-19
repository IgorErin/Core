module Utils where 

mapAccum :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccum _ acc [] = (acc, []) 
mapAccum f acc (hd : tl) = (acc'', hd' : tl')
    where 
        (acc', hd') = f acc hd
        (acc'', tl') = mapAccum f acc' tl   

     