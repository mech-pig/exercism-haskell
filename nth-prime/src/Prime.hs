module Prime (nth) where

import Control.Category ((>>>))

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (iterate (succ >>> nextPrime) 1 !! n)

nextPrime :: Integer -> Integer
nextPrime = until isPrime succ

isPrime :: Integer -> Bool
isPrime n = hasNoIntegerDivisorsStartingFrom 2
  where
    hasNoIntegerDivisorsStartingFrom :: Integer -> Bool
    hasNoIntegerDivisorsStartingFrom i
      | (i * i) > n = True
      | otherwise   = (n `mod` i /= 0) && hasNoIntegerDivisorsStartingFrom (i + 1)
