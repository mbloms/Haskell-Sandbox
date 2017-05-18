
import Data.Word
import Data.Bits 

type Bitset = [Word64]


nil :: Bitset
nil = complement 3 : repeat (complement 0)

eliminate :: Int -> Int -> Bitset -> Bitset
--eliminate _ 64 (b:_) = b:[]
eliminate p i (b:bs) =
	if i >= 64
		then b:eliminate p (i-64) bs
		else eliminate p (i+p) (clearBit b i :bs)

bits p = eliminate p 0 nil

showBits :: FiniteBits a => a -> String
showBits n = take (finiteBitSize n) $ map f $ map (testBit n) [0..]
	where
		f True = '1'
		f False = '0'

next :: Bitset -> Int
next (0:bs) = 64+ next bs
next (b:_) = countTrailingZeros b

main = do
	let p = 2
	print p
	let bs = eliminate p 0 nil
	main' bs

main' bs = do
	let p = next bs
	print p
	let bs' = eliminate p 0 bs
	main' bs'