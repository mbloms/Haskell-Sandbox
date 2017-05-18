import qualified Data.Vector as V

data Tree a = Null | Branch Tree a a Tree a deriving Show

qsort :: Ord a => Vector a -> Tree (Vector a)
qsort v = if V.null v
	then Null
	else Branch qsort prefix mid suffix
	
	where
		(prefix,rest) = partition (< V.head v) v
		(mid,suffix) = span (== V.head rest) rest