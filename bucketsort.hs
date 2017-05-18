import Data.Array
import Data.List

test = bucketSort [0..99999]

bucketSort lst = (concat.elems) $ accumArray (flip (:)) [] (0,k) [(i e,e) | e <- lst]
	where
		k = 999
		i e = min k $ max 0 $ quot e k