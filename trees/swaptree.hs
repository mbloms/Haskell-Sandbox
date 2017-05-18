

data SwapHeap a = Empty | a :< (SwapHeap a,SwapHeap a) | a := (SwapHeap a,SwapHeap a)

singleton a = a := (Empty,Empty)

top (a:<_) = a
top (a:=_) = a

push x Empty = x := (Empty,Empty)
push x (t:<(as,bs)) = (mi:=(push mx as, bs))
		where (mi,mx) = ord (x,t)

push x (t:=(as,bs)) =
		if top as < top bs
		then
			(mi:<(as,push mx bs))
		else
			(mi:<(bs,push mx as))
	where (mi,mx) = ord (x,t)

ord (a,b) = if a<b then (a,b) else (b,a)

pop (t:=(Empty,Empty)) = (t,Empty)
pop (t:<(Empty,ts)) = (t,ts)
pop (t:=(left,right)) = if top left > top right
	then
		(t,((fst.pop) right :< ((snd.pop) right, left)))
	else
		(t,((fst.pop) left :< ((snd.pop) left, right)))

pop (t:<(left,right)) = if top left > top right
	then 
		(t,((fst.pop) right := (left, (snd.pop) right)))
	else
		(pop.balance) (t:<(left,right))


balance (left,right) = ((bubble.swap) (top right) left, (bubble.swap) (top left) right)

swap x (t:=tree) = (t,x:=tree)
swap x (t:<tree) = (t,x:<tree)

bubble (t:=tree) = let (a,b) = bubbleh t tree in a:=b
bubble (t:<tree) = let (a,b) = bubbleh t tree in a:<b
bubble (t:<(Empty,t2)) = push t t2

bubbleh t (Empty,Empty) = (t,(Empty,Empty))
bubbleh t (left,right)
	| t < min (top left) (top right) = (t,(left,right))
	| top left < top right = let (l,tree) = swap t left in (l,(bubble tree,right))
	| otherwise = let (r,tree) = swap t right in (r,(left, bubble tree))



--cons :: SwapHeap a -> (a, SwapHeap a)
--cons (last:<(Empty,Empty)) = (last,Empty)
--cons (a:<(left,Empty)) = (last,(a:<(Empty,tree)))
--	where (last,tree) = initr left
--
--cons (a:<(left,right)) = (last,(a:<(l,tree)))
--	where (last,tree) = initr right
--
--tick Empty = Empty
--tick (t:<(a,b)) = (t:<(b,tick a))