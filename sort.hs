
scan :: Ord a => [a] -> ([a]->[a]) -> a -> a -> [[a]]
scan (x:xs) acc min max
    | max <= x = scan xs (\ts-> acc (x:ts)) min x
    | x < min = scan xs (\ts-> x: acc ts) x max
    | otherwise = (acc []): scan xs (x:) x x
scan _ acc _ _ = [acc []]

merge :: Ord a => [a] -> [a] -> [a]
merge (a:as) (b:bs)
    | a <= b = a: merge as (b:bs)
    | otherwise = b: merge (a:as) bs
merge [] rest = rest
merge rest [] = rest

mergeList :: Ord a => [[a]] -> [a]
mergeList [merged] = merged
mergeList unmerged = mergeList.mergeAll $ unmerged
    where
        mergeAll :: Ord a => [[a]] -> [[a]]
        mergeAll (a:b:xs) = merge a b : mergeAll xs

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = mergeList $ scan xs (x:) x x