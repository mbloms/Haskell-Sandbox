import Data.List hiding (nub)

main = do
	input <- getContents
	let (arg:ps:_) = lines input
	let w = head $ map read (words arg) :: [Int]
	let partitions = 0:w:map read (words ps):: [Int]
	putStrLn $ showlst (nub $ sort [a-b|a<-partitions,b<-partitions,b<a])

showlst :: [Int] -> String
showlst ints = intercalate " " (map show ints)

nub :: [Int] -> [Int]
nub lst = map head (group lst)