
qsort :: [a] -> [a]
qsort lst = qsort lst []

qsort' :: [a] -> [a] -> [a]
qsort' (p:xs) = \next-> qsort' less (p:qsort' great next)
	where (less, great) = partition (<p) xs

qsort' [] = id

data Tree a = Branch a (Tree a) (Tree a) (a,a) | Leaf

insert x Leaf = Branch x Leaf Leaf (x,x)
insert x tree@(Branch p left right (min,max))
	| x < p
		| x < min = Branch x Leaf tree (x,max)
		| otherwise = Branch p (insert x left) right (min,max)
	| otherwise
		| max < x = Branch x tree Leaf (min,x)
		| otherwise = Branch p left (insert x right) (min,max)

toCons Leaf = id
toCons (Branch p left right _) = \next -> toCons left (p:toCons right next)

toList t = toCons t []

