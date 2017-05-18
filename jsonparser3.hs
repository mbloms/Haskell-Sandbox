{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
--
--p = T.pack
--
--main = T.interact $ T.unlines.map T.unlines.map (T.splitOn ("\":\"")).T.lines


--f = (groupOn (flip elem ":\\\""))
--f = groupOn (=='\"')


		--f a = unlines $ map show $ zip (f' a) (filter (elem "{},:\n\"\\") a)
		--f' a = map (findIndices (flip elem "{},:\n\"\\")) $ lines a


infixr 6 :/
data JSEntry = String :/ JSValue deriving (Show, Read)
data JSValue = JSTrue | JSFalse | Null deriving (Show, Read)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn x = groupBy (flip((==).x).x)

data JSToken = LeftCurly Int
			 | RightCurly Int
			 | LeftBracket Int
			 | RightBracket Int
			 | Comma
			 | Colon
			 | Quote
			 | JSString T.Text
			 deriving (Show, Read)

infixr 6 =:
(=:) x xs = x == T.head xs

main = do
	line <- T.getLine
	mapM_ print $ splitQuote line

splitQuote :: T.Text -> [T.Text]
splitQuote t = unmaybe splitted
	where
		qq (_,'\\','\"',_) = False
		qq (_,_,'\"',_) = True
		qq _ = False
		splitted = find qq $ (zip4 ((tail.T.inits) t) (T.unpack t) ((tail.T.unpack) t) ((tail.tail.T.tails) t))
		unmaybe Nothing = [t]
		unmaybe (Just (a,_,_,b)) = a: splitQuote b

parse :: [T.Text] -> [[JSToken]]
parse (a:b:cs) = ts:[JSString b]:parse cs
	where (i,ts) = parse' i a
parse (a:[]) = parse' a:[]
parse [] = []

parse' :: Int -> T.Text -> [JSToken]
parse' i text = case T.head text of
	'{' -> LeftBracket



{-
*Main> print $ find qq $ (\t -> (zip4 ((tail.T.inits) t) (T.unpack t) ((tail.T.unpack) t) ((tail.tail.T.tails) t))) "and\"reas"
Just ("and",'d','"',"reas")
*Main> let qq (_,'\\','\"',_) = False; qq (_,_,'\"',_) = True; qq _ = False
-}
