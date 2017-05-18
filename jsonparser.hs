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
			 | Strict T.Text
			 deriving (Show, Read)

infixr 6 =:
(=:) x xs = x == T.head xs

parseQuotes :: T.Text -> [JSToken]
parseQuotes str
	| T.null str = []
	| '\"' =: str = Quote : parseQuotes (T.tail str)
	| '\\' =: str = 
		let
			i = T.findIndex (flip elem "\\\"") (T.drop 2 str)
			(a,b) = if isJust i then T.splitAt ((fromJust i)+2) str else (str,T.empty)
		in Strict a : parseQuotes b
	| otherwise = 
		let
			i = T.findIndex (flip elem "\\\"") str
			(a,b) = if isJust i then T.splitAt (fromJust i) str else (str,T.empty)
		in Strict a : parseQuotes b

--findQuotes :: T.Text -> [Int]
--findQuotes x = helper 0 x
--	where
--		helper i ts = if T.null ts then [] else
--			case (T.head ts) of
--				'\\' -> helper (i+2) (T.drop 2 ts)
--				'\"' -> i : helper 0 (T.tail ts)
--				_ -> helper (i+1) (T.tail ts)
--
--findTokens :: T.Text -> [(Int,JSToken)]
--findTokens x = helper 0 x
--	where
--		helper i ts = if T.null ts then [] else
--			case (T.head ts) of
--				'\\' -> helper (i+2) (T.drop 2 ts)
--				'\"' -> i : helper 0 (T.tail ts)
--				_ -> helper (i+1) (T.tail ts)

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


{-
*Main> print $ find qq $ (\t -> (zip4 ((tail.T.inits) t) (T.unpack t) ((tail.T.unpack) t) ((tail.tail.T.tails) t))) "and\"reas"
Just ("and",'d','"',"reas")
*Main> let qq (_,'\\','\"',_) = False; qq (_,_,'\"',_) = True; qq _ = False
-}
