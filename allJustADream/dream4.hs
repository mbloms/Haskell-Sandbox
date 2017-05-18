import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex)


data Line = E Event | D Int | S [(Bool,Event)] deriving Show

type Event = BS.ByteString

readBangEvent :: BS.ByteString -> (Bool,Event)
readBangEvent bs = case BS.uncons bs of
    Just ('!',tail) -> (False,tail)
    _ -> (True,bs)

readLine :: BS.ByteString -> Line
readLine bsline = case BS.head bsline of
    'E' -> E (BS.drop 2 bsline)
    'D' -> D (readInt (BS.drop 2 bsline))
    'S' -> S events
        where
            (_:_:tokens) = BS.words bsline
            events = map readBangEvent tokens

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

type EventStack = [(Int,(HM.HashMap Event Int))]

push :: Event -> EventStack -> EventStack
push e [] = [(1,HM.singleton e 1)]
push e es@((i,t):_) = (i+1,(HM.insert e (i+1) t)) : es

length' :: EventStack -> Int
length' [] = 0
length' ((i,_):_) = i

process :: EventStack -> [Line] -> [Answer]
process _ [] = []
process events (l:ls) = case l of
    E event -> process (push event events) ls
    D d -> process (drop d events) ls
    S bes -> testScenario (1+length' events) 0 bes events : process events ls

testScenario :: Int -> Int -> [(Bool,Event)] -> EventStack -> Answer
testScenario _ 0 [] _ = Yes
testScenario _ throw [] _ = JustADream throw

testScenario keep throw bes [] = case filter fst bes of
    [] -> case throw of
        0 -> Yes
        _ -> JustADream throw
    _ -> PlotError

testScenario keep throw ((True,event):bes) events =
    case elemIndex' event events of
        Nothing -> PlotError
        Just i -> 
            if (i+1) >= keep --Om vi redan räddat
                then testScenario keep throw bes events
                else if (i+1) > throw --
                    then testScenario (i+1) throw bes events
                    else PlotError

testScenario keep throw ((False,event):bes) events =
    case elemIndex' event events of
        Nothing -> testScenario keep throw bes events
        Just i -> 
            if (i+1) <= throw --Om vi redan kastat bort
                then testScenario keep throw bes events
                else if (i+1) < keep --Om vi vill kasta bort mindre än behålla
                    then testScenario keep (i+1) bes events
                    else PlotError


elemIndex' :: BS.ByteString -> EventStack -> Maybe Int
elemIndex' key ((size,trie):_) = fmap (size -) (HM.lookup key trie)
