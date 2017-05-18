
type LambdaList a = [a] -> [a]

backCons :: LambdaList a -> a -> LambdaList a
backCons xf x = \next -> xf (x:next)

data Queue a = Queue [a] (LambdaList a)

peak :: Queue a -> a
peak (Queue (x:_) _) = x
peak (Queue _ back) = head (back (error "Empty Queue."))

pop :: Queue a -> Queue a
pop (Queue (_:xs) back) = Queue xs back
pop (Queue _ back) = case back [] of
	(_:xs) -> Queue xs id
	_ -> error "Empty Queue."

put :: Queue a -> a -> Queue a
put (Queue [] back) x = Queue (back []) (x:)
put (Queue front back) x = Queue front (\next -> back (x:next))
