import Data.List

data List a = Cons a (List a)
				| Nil
					deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList Nil = []
toList (Cons x r) = x : toList r


data Tree2 a = Node a (Maybe (Tree2 a)) (Maybe (Tree2 a))
					deriving (Show)


lenList :: [a] -> Int
lenList [] = 0
lenList (h:t) = 1 + lenList t

mean :: [Float] -> Float
mean t = (sum t) / (fromIntegral $ length t)


palindromize xs = xs ++ reverse xs

isPalindrome x = x == reverse x

isPalindromeRec xs | length xs < 1 = True
						 | head xs == last xs = isPalindromeRec . tail . init $ xs
						 | otherwise = False


sortByLength xs = sortBy (\x y -> compare (length x) (length y)) xs

myintersperse :: a -> [[a]] -> [a]
myintersperse sep (l:[]) = l
myintersperse sep (h:t) = h ++ [sep] ++ (myintersperse sep t)


data Tree a = Item a (Tree a) (Tree a)
				  | Empty


treeHeight :: Tree a -> Int
treeHeight tree = 
	case tree of 
		Empty -> 0
		Item a t1 t2 -> 1 + max (treeHeight t1) (treeHeight t2) 


data Direction = DLeft
					  | DRight
					  | Straight
					deriving (Show)

crossp :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
crossp (x1,y1) (x2,y2) (x3,y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3-x1)			
getDirection :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Direction
getDirection a b c = calcdir $ crossp a b c 
		where	calcdir diff 
					| diff == 0 = Straight
					| diff > 0  = DLeft
					| diff < 0  = DRight
				
