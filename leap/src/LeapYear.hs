module LeapYear (isLeapYear) where

import Control.Category ((>>>))
import Control.Applicative (liftA2)

{-
I'm using an Applicative-based solution that allows one to compose all the required
validation checks in a declarative fashion.

Inspirations:
- https://stackoverflow.com/a/841931
- https://stackoverflow.com/a/5710132
-}

isLeapYear :: Integer -> Bool
isLeapYear = (isDivisibleBy 400) <*||*> ((isDivisibleBy 4) <*&&*> (not . (isDivisibleBy 100)))

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy n = flip mod n >>> ((==) 0)

(<*&&*>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<*&&*>) = liftA2 (&&)

(<*||*>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<*||*>) = liftA2 (||)
