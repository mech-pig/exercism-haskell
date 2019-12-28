module Luhn (isValid) where

import Control.Category ((>>>))
import Data.Char        (digitToInt, isDigit, isSpace)

isValid :: String -> Bool
isValid = stripSpaces >>> (\s -> if (doesItContainNonDigitChars s) then False else (parseDigits >>> verifyLuhnParity) s)

stripSpaces :: String -> String
stripSpaces = filter (not . isSpace)

doesItContainNonDigitChars :: String -> Bool
doesItContainNonDigitChars = any (not . isDigit)

parseDigits :: String -> [Int]
parseDigits = map digitToInt

verifyLuhnParity :: [Int] -> Bool
verifyLuhnParity s
  | length s <= 1 = False
  | otherwise     = (reverse >>> doubleEverySecond >>> sum >>> (isDivisibleBy 10)) s

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond = transformEveryNth doubleBound 2

doubleBound :: Int -> Int
doubleBound = (* 2) >>> (\n -> if n > 9 then n - 9 else n)

transformEveryNth :: (a -> a) -> Int -> [a] -> [a]
transformEveryNth f n = (zip [ 1 .. ]) >>> (map (\(i, x) -> apply f n i x))
  where
    apply :: (a -> a) -> Int -> Int -> a -> a
    apply f n i x
      | isDivisibleBy n i = f x
      | otherwise         = x

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy m n = (n `mod` m) == 0
