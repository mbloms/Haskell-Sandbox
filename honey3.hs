import Data.List
import Data.Array
import Data.Maybe

main = interact mainf

mainf :: String -> String
mainf stdin = unlines $ map (show.(flip ways (0,0))) ns
    where
        i:input = map read (lines stdin) :: [Integer]
        ns = take (fromInteger i) input

ways :: Integer -> (Integer, Integer) -> Integer
ways 0 (0,0) = 1
ways 0 _ = 0
ways n cell = sum $ map (ways' (n-1)) $ branch cell

lim = 14
ways' i (x,y) = fromJust $ lookup (i,x,y) arr
arr = zip is $ map tways is
	where
		is = [(a,b,c)| a <- [0..lim], b <- [-lim..lim], c <- [-lim..lim]]
		tways (a,b,c) = ways a (b,c)

branch :: (Integer,Integer) -> [(Integer,Integer)]
branch cell = map (add cell) directions

directions :: [(Integer,Integer)]
directions = [(-1,-1),(-1,0),(0,-1),(0,1),(1,0),(1,1)]

add :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
add (x,y) (a,b) = (x+a,y+b)