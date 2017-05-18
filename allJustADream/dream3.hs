
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as WBS
import Data.Word
import Data.List (elemIndex, partition)
import Data.Maybe (isJust)


data Line = E Event | D Int | S ([Event],[Event]) deriving Show

type Event = BS.ByteString

readLine :: BS.ByteString -> Line
readLine bsline = case BS.head bsline of
    'E' -> E (BS.drop 2 bsline)
    'D' -> D (readInt (BS.drop 2 bsline))
    'S' -> S (true,map BS.tail untrue)
        where
            (_:_:tokens) = BS.words bsline
            (true,untrue) = partition bangstr tokens
            bangstr bstr = case BS.uncons bstr of
                Just ('!',_) -> False
                _ -> True

--getInts = fmap (map (BS.foldl (\acc-> \w-> acc*10-48+fromEnum w) 0). BS.split 32) BS.getLine
readInt :: BS.ByteString -> Int
readInt = (BS.foldl (\acc-> \w-> acc*10-48+fromEnum w) 0)
--readInt = read.BS.unpack

showAnswer :: Answer -> BS.ByteString
showAnswer Yes = yes
showAnswer (JustADream i) = BS.unwords [(BS.pack.show) i, justADream]
showAnswer PlotError = plotError

[yes,justADream,plotError] = map BS.pack ["Yes","Just A Dream","Plot Error"]

main = BS.interact $
    BS.unlines . 
    map showAnswer . 
    process [] . 
    map readLine .
    filter filterfunc.
    BS.lines

filterfunc bs = case BS.uncons bs of
    Nothing -> False
    Just ('E',_) -> True
    Just ('D',_) -> True
    Just ('S',_) -> True
    _ -> False

data Answer = Yes | JustADream Int | PlotError deriving Show

type EventStack = [(Int,(Trie Int))]

push :: Event -> EventStack -> EventStack
push e [] = [(1,singleton e 1)]
push e es@((i,t):_) = (i+1,(insertT e (i+1) t)) : es

length' :: EventStack -> Int
length' [] = 0
length' ((i,_):_) = i

process :: EventStack -> [Line] -> [Answer]
process _ [] = []
process events (l:ls) = case l of
    E event -> process (push event events) ls
    D d -> process (drop d events) ls
    S bes -> testScenario bes events : process events ls

testScenario :: ([Event],[Event]) -> EventStack -> Answer
testScenario (true,_) [] = case true of
    [] -> Yes
    _ -> PlotError
testScenario (true,untrue) events = case rest of
    ((_,trie):_) -> if and $ map (isJust.(flip lookupT) trie) true
        then res
        else PlotError
    _ -> case true of
        [] -> res
        _ -> PlotError
    where
        (d,rest) = testUntrue untrue (0,events)
        res = if d == 0 then Yes else JustADream d

testUntrue :: [Event] -> (Int,EventStack) -> (Int,EventStack)
testUntrue [] events = events
testUntrue (event:untrue) (i,events) = case elemIndex' event events of
    Nothing -> testUntrue untrue (i,events)
    Just d -> testUntrue untrue (i+d+1, drop (d+1) events)


data Trie a = Singleton BS.ByteString a | Noval [Subtrie a] | Val a [Subtrie a] deriving Show
type Subtrie a = (Word8,Trie a)

null (Noval []) = True
null _ = False
empty = Noval []
singleton bs val = if BS.null bs then Val val [] else Singleton bs val

elemIndex' :: BS.ByteString -> EventStack -> Maybe Int
elemIndex' key ((size,trie):_) = fmap (size -) (lookupT key trie)

lookupT :: BS.ByteString -> Trie a -> Maybe a
lookupT key (Singleton bs val) = if key == bs
    then Just val
    else Nothing

lookupT bs trie = case WBS.uncons bs of
    Nothing -> (fst.unconsT) trie
    Just (c,cs) -> case (lookup c.snd.unconsT) trie of
        Nothing -> Nothing
        Just subtrie -> lookupT cs subtrie

insertT :: BS.ByteString -> a -> Trie a -> Trie a
insertT key val trie = 
    case unconsT trie of
        (Nothing,[]) -> singleton key val
        (root,children) -> case WBS.uncons key of
            Nothing -> consT (Just val) children
            Just (b,bs) -> consT root (inserth b bs val children)

inserth :: Word8 -> BS.ByteString -> a -> [Subtrie a] -> [Subtrie a]
inserth k key val [] = [(k,singleton key val)]
inserth k key val ((c,t):cs) = case k `compare` c of
    LT -> (k,singleton key val):(c,t):cs
    EQ -> (c,insertT key val t) : cs
    _ -> (c,t) : inserth k key val cs



consT :: Maybe a -> [Subtrie a] -> Trie a
consT (Just val) cs = Val val cs
consT Nothing cs = Noval cs

unconsT :: Trie a -> (Maybe a,[Subtrie a])
unconsT (Noval cs) = (Nothing, cs)
unconsT (Val val cs) = (Just val, cs)
unconsT (Singleton bs val) = case WBS.uncons bs of
    Nothing -> (Just val,[])
    Just (c,cs) -> (Nothing,[(c,singleton cs val)])
