

data Queueback a = Empty | Back (Queueback a) (Ffix a) deriving Show

data Ffix a = F1 ([a] -> [a])
    | F2 ([a] -> [a])
    | F3 ([a] -> [a])
    | F4 ([a] -> [a])

fsnoc :: Ffix a -> ([a] -> [a]) -> Ffix a
fsnoc (F1 fs) f = F2 (fs.f)
fsnoc (F2 fs) f = F3 (fs.f)
fsnoc (F3 fs) f = F4 (fs.f)
fsnoc (F4 fs) f = F4 (fs.f)

feval (F1 fs) = fs
feval (F2 fs) = fs
feval (F3 fs) = fs
feval (F4 fs) = fs

instance (Show a) => Show (Ffix a) where
    show (F1 fs) = show (fs [])
    show (F2 fs) = show (fs [])
    show (F3 fs) = show (fs [])
    show (F4 fs) = show (fs [])

qsnoc :: Queueback a -> ([a] -> [a]) -> Queueback a
qsnoc (Back q (F4 suffix)) x = Back (qsnoc q suffix) (F1 x)
qsnoc (Back q suffix) x = Back q (fsnoc suffix x)
qsnoc Empty x = Back Empty (F1 x)

qeval :: Queueback a -> ([a] -> [a])
qeval Empty = id
qeval (Back q suffix) = (qeval q).(feval suffix)

newtype Queue a = Queue ([a],(Queueback a)) deriving Show

infixl 5 |>
infixr 5 <|

(|>) :: Queue a -> a -> Queue a
(Queue (flst,q)) |> x = Queue (flst,qsnoc q (x:))

(<|) :: a -> Queue a -> Queue a
x <| (Queue (flst,q)) = Queue (x:flst,q)

uncons :: Queue a -> Maybe (a,Queue a)
uncons (Queue ([],Empty)) = Nothing
uncons (Queue ([],q)) = uncons (Queue (qeval q [],Empty))
uncons (Queue ((x:xs),q)) = Just (x,Queue (xs,q))
