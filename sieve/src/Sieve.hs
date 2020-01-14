module Sieve (primesUpTo) where

import Control.Category ((>>>))
import Data.List ((\\))
-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))



primesUpTo :: Integer -> [Integer]
primesUpTo n = [2 .. n] \\ do
    i <- [ 2..(baseLimit n) ]
    generateMultiples n i 0
    where
      baseLimit :: Integer -> Integer
      baseLimit = realToFrac >>> sqrt >>> floor

      generateMultiples :: Integer -> Integer -> Integer -> [Integer]
      generateMultiples limit base step =
        if candidate > limit
          then []
          else candidate : generateMultiples limit base (step + 1)
        where
          candidate = (base^2) + (base*step)
