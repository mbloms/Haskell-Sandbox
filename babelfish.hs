import Data.Maybe
import Data.Map.Strict hiding (map)
import Data.List hiding (lookup)
import Prelude hiding (lookup)
main = interact babelfish

babelfish :: String -> String
babelfish input = unlines $ map (fromMaybe "eh") $ map (flip lookup (fromList dict)) mess
    where
        (transl,(m:mess)) = break (=="") $ lines input
        dict = map ((\[x,y] -> (y,x)).words) transl