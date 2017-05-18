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
parseQuotes str =
	where
		triple x = let (a,b) = break (=='\"') x in (init a,last a,tail b)
		escaped (t:ts) = case triple t of
			(bef,'\\',aft) -> 