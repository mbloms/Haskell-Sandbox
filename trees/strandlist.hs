import Data.List
import System.Random

data StrandList a = Empty | a:<(StrandList a, StrandList a)

append :: Ord a => StrandList a -> StrandList a -> StrandList a
append tree Empty = tree
append (t:<(Empty,descending)) tree = t:<(larger, append descending smaller)
	where
		(smaller, larger) = spanStrand (<t) tree

append (t:<(ascending,descending)) tree = t:<(append ascending tree, descending)
append Empty tree = tree

singleton x = (x:<(Empty,Empty))

cons :: Ord a => a -> StrandList a -> StrandList a
cons x = append (singleton x)

fromList :: Ord a => [a] -> StrandList a
fromList = foldr cons Empty

toList :: Ord a => StrandList a -> [a]
toList Empty = []
toList (x:<(asc,desc)) = x:toList desc ++ toList asc

extractStrand :: Ord a => StrandList a -> ([a],StrandList a)
extractStrand slist = (lst, rest)
	where
		(lst, rests) = (unzip.sever) slist
		rest = foldr1 append rests

strands Empty = []
strands slist = strand : strands rest
	where
		(strand, rest) = extractStrand slist

spanStrand :: (a -> Bool) -> StrandList a -> (StrandList a, StrandList a)
spanStrand p = (\(a,b) -> (unsever a, unsever b)).span (p.fst).sever

sever :: StrandList a -> [(a,(StrandList a))]
sever Empty = []
sever (x:<(xs,ys)) = (x,ys):sever xs

unsever :: [(a,(StrandList a))] -> StrandList a
unsever [] = Empty
unsever ((x,ys):xs) = (x:<(unsever xs,ys))