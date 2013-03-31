{-Classifier inspired by Tomas Petricek classifier DSL 
 - https://github.com/tpetricek/Documents/tree/master/Talks%202013/Domain%20Specific%20Languages%20(NYC)/code/Classifiers
 -}

module Classifier where 
import Data.Time.Calendar
import Data.List

type Date = Day
type Value = Double
type Quote = (Date,Value)

newtype Classifier a = Classify { runClassifier ::  [Quote] -> a }

unit :: a -> Classifier a
unit v = Classify $ \_ -> v

bind :: Classifier a -> (a -> Classifier b) -> Classifier b
bind m f = Classify $ \vs -> let x = runClassifier m vs
                             in  runClassifier (f x) vs

both :: Classifier a -> Classifier b -> Classifier (a,b)
both f g = Classify $ \vs -> (runClassifier f vs , runClassifier g vs) 


justValues :: [Quote] -> [Value]
justValues = map snd

reduce :: (Value -> Value -> Value) -> Classifier Value
reduce f = Classify $ foldl1' f . justValues

collapse :: ([Value] -> Value) -> Classifier Value
collapse f = Classify $ f . justValues

diff = collapse calcDiff 
    where calcDiff [] = 0/0
          calcDiff [x] = 0/0
          calcDiff (x:xs) = (last xs) - x

pctDiff = collapse calcPctDiff
    where calcPctDiff (x:xs) = (last xs) / x - 1.0

mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral $ length xs)


--average = Classify $ \vs -> (sum $ map snd vs) / (fromIntegral $ length vs)
average = collapse mean

instance Monad Classifier where
    return = unit
    (>>=) = bind

instance Functor Classifier where
    fmap f (Classify a) = Classify $ f . a
    

-- This needs to be factored out to a utility library
windowed :: Int -> [a] -> [[a]]
windowed num [] = []
windowed num ls@(x:xs)
    | len < num = []
    | otherwise = (take num ls) : (windowed num xs)
    where len = length ls

window :: Int -> Classifier a -> Classifier [a]
window num first = Classify $ \vs -> map (runClassifier first) (windowed num vs)

filter :: (a -> Bool) -> Classifier [a] -> Classifier [a]
filter predicate first = Classify $ \vs -> Data.List.filter predicate $ runClassifier first vs
{-  
 - 
 -
 -
 -  -}
