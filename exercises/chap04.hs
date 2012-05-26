import Data.Char
import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:t) = Just h

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (h:t) = Just t

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let (a,b) = break p xs in 
					  a : splitWith p (drop 1 b)

asInt_fold :: String -> Int
asInt_fold "" = error "Must not be empty string"
asInt_fold "-" = error "Not a digit"
asInt_fold ('-':str) = -1 * asInt_fold str 
asInt_fold str = foldl' (\s c -> if c == '.' then 
                                    error "Can't convert decimal" 
                                 else 
                                    s*10 + digitToInt c) 0 str

myconcat :: [[a]] -> [a]
myconcat xs = foldr (\s x -> s ++ x) [] xs


mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile pred (h:t) = if pred h then 
                           h : mytakeWhile pred t 
                       else 
                           []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr pred xs = snd $ foldr myfilter (False,[]) xs
   where myfilter x (False, ys) = (pred x, [x])
         myfilter x (True, ys) = (pred x,x:ys)
