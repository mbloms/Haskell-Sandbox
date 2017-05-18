import Data.Monoid
import Data.List
--import GHC.Exts (IsList(..))





newtype Funclist a = Funclist ([a] -> [a])

instance Monoid (Funclist a) where
    mempty = Funclist id
    Funclist a `mappend` Funclist b = Funclist (a.b)

instance Functor Funclist where
    fmap f (Funclist flst) = Funclist (map f (flst [])++)

instance Show a => Show (Funclist a) where
    show (Funclist lst) = "Funclist "++ show (lst [])

--{-# LANGUAGE TypeFamilies #-}
----{-# LANGUAGE OverloadedLists #-}
--instance IsList (Funclist a) where
--    type Item (Funclist a) = a
--    toList (Funclist flst) = flst []
--    fromList xs = Funclist (xs++)

toList (Funclist flst) = flst []
--fromList xs = Funclist (xs++)

--fromList (x:xs) = (x:) .> fromList xs
--fromList [] = Funclist id
fromList :: [a] -> Funclist a
fromList = Funclist . foldl1 (.) . map (:)

f .> (Funclist lst) = Funclist (f.lst)
(Funclist lst) <. f = Funclist (lst.f)

loop :: a -> a
loop = loop

looplst :: [a]
looplst = looplst

snoc :: Funclist a -> a -> Funclist a
snoc blst x = blst <. (x:)

cons :: a -> Funclist a -> Funclist a
cons x blst = (x:) .> blst

uncons :: Funclist a -> (a,Funclist a)
uncons blst = (head (toList blst), tail.>blst)

unsnoc :: Funclist a -> (Funclist a, a)
unsnoc blst = (init.>blst, last (toList blst))

