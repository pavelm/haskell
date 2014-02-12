import Numeric.Probability.Distribution 

shuffleM :: (Num prob, Fractional prob) => [a] -> T prob [a]
shuffleM [] = return []
shuffleM [x] = return [x]
shuffleM (pivot:li) = do 
        (left, right) <- partition li
        sleft <- shuffleM left
        sright <- shuffleM right
        return (sleft ++ [pivot] ++ sright)
    where partition [] = return ([],[])
          partition (x:xs) = do
                    (left,right) <- partition xs
                    uniform [(x:left, right),(left,x:right)]
