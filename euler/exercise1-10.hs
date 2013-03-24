import Data.List

-- Common functionality
--
primes = nubBy (((==0).).rem) [2..]

isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

primeFactors n = factors n primes
 where
   factors 1 _                  = []
   factors m (p:ps) | m < p*p   = [m]
                    | r == 0    = p : factors q (p:ps)
                    | otherwise = factors m ps
       where (q,r) = quotRem m p

problem1 max = sum  [ x | x <- [ 1 .. max-1 ] , x `mod` 3 == 0 || x `mod` 5 == 0 ]

problem2 max = sum $ takeWhile (< max) $ filter even fibs
    where fibs = unfoldr fibGen (0,1)
          fibGen (a,b) = Just (a, (b,a+b))

problem3 = maximum $ primeFactors 600851475143


