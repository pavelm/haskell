import Data.List

fac 0 = 1
fac 1 = 1
fac n = n * (fac ( n - 1 ) )

--permute accum count 1  = permute (1 : accum) (count - 1) 1
permute accum count 0  = replicate count 0 ++ accum
permute accum count index = 
    permute (newIndex : accum) (count - 1) rem
    where 
        myfac = fac $ count - 1
        newIndex = index `div` myfac
        rem = index `mod` myfac

removeIndex idx list = 
    concat [one, tail two] 
    where (one, two) = splitAt idx list

scanfun list idx = 
    (removeIndex idx list, list !! idx)

findPermutations values ith =
    snd $ mapAccumL scanfun values (reverse $ permute [] (length values) ith) 
