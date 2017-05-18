import System.Random (randomRs, mkStdGen)
import Data.Maybe


strand :: Ord a => a -> [a] -> ([a],[a])
strand _ [] = ([],[])
strand n (x:xs)
	| x < n = (a,x:b)
	| otherwise = (x:a,b)
	where
		(a,b) = strand (max x n) xs

strands :: Ord a => [a] -> [[a]]
strands [] = []
strands (u:us) = a:strands b
	where
		(a,b) = strand u us

showlines str = putStr $ unlines $ map show str

weakstrand :: Ord a => a -> [a] -> [[a]]
weakstrand _ [] = []
weakstrand n (x:xs)
	| n <= x = ((x:t):ts)
	| otherwise = ([]:(x:t):ts)
	where
		tts = weakstrand x xs
		(t:ts) = if tts == [] then ([]:[]) else tts