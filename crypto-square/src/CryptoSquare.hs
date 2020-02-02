module CryptoSquare (encode) where

import Control.Category ((>>>))
import Control.Monad    (guard)
import Data.Char        (isAlphaNum, toLower)


encode :: String -> String
encode = normalize

normalize :: String -> String
normalize xs = do
  c <- xs
  guard (isAlphaNum c)
  return (toLower c)

calculateChunkLength :: String -> Integer
calculateChunkLength = length >>> fromIntegral >>> sqrt >>> ceiling

calculateNumberOfChunks :: Integer -> String -> Integer
calculateNumberOfChunks chunckLength = length -> (div)
