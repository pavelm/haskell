{-Classifier inspired by Tomas Petricek classifier DSL 
 - https://github.com/tpetricek/Documents/tree/master/Talks%202013/Domain%20Specific%20Languages%20(NYC)/code/Classifiers
 -}

module Data.Classifier(Classifier) where 
import Data.DateTime

newtype Classifier a = Classify { runClassifier ::  [(DateTime,Double)] -> a }

unit :: a -> Classifier a
unit v = Classify $ \_ -> v

bind :: Classifier a -> (a -> Classifier b) -> Classifier b
bind m f = Classify $ \vs -> let x = runClassifier m vs
                             in  runClassifier (f x) vs

both :: Classifier a -> Classifier b -> Classifier (a,b)
both f g = Classify $ \vs -> (runClassifier f vs , runClassifier g vs) 


example = [ (fromSeconds $ 1364240000 + s, fromIntegral s :: Double) | s<- [0,1000 .. 100000]]

average = Classify $ \vs -> (sum $ map snd vs) / (fromIntegral $ length vs)

instance Monad Classifier where
    return = unit
    (>>=) = bind
