{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Acronym (abbreviate, defaultConfiguration, evaluateChar, ProgramConfiguration(..), ProgramState(..), splitWords) where

import           Data.Char (isAlpha, isSpace, isLower, isUpper)
import qualified Data.Text as T
import           Data.Text (Text)


isEmpty :: Text -> Bool
isEmpty = (== T.empty)

splitWords :: Text -> [Text]
splitWords = (\(ProgramState groupedWords lastGroup) -> groupedWords ++ [lastGroup]) . T.foldl (evaluateChar defaultConfiguration) (ProgramState [] "")

abbreviate :: Text -> Text
abbreviate = T.intercalate "" . map (T.toUpper . T.take 1) . splitWords

data ProgramState = ProgramState [Text] Text
  deriving (Eq, Show)

data ProgramConfiguration = ProgramConfiguration
  { isSeparator       :: Char -> Bool
  , shouldBeDiscarded :: Char -> Bool
  , shouldSplit       :: Char -> Char -> Bool
  }

defaultConfiguration :: ProgramConfiguration
defaultConfiguration = ProgramConfiguration
  { isSeparator       = \c -> any ($ c) [isSpace, (== '-'), (== '\t')]
  , shouldBeDiscarded = not . isAlpha
  , shouldSplit       = \lastChar currentChar -> isLower lastChar && isUpper currentChar
  }

evaluateChar :: ProgramConfiguration -> ProgramState -> Char -> ProgramState
evaluateChar ProgramConfiguration { isSeparator, shouldBeDiscarded, shouldSplit } (ProgramState groupedWords pendingGroup) currentChar
  | isSeparator currentChar                       = ProgramState (appendNonEmpty groupedWords pendingGroup) T.empty
  | shouldBeDiscarded currentChar                 = ProgramState groupedWords pendingGroup
  | isEmpty pendingGroup                          = ProgramState groupedWords (T.singleton currentChar)
  | shouldSplit (T.last pendingGroup) currentChar = ProgramState (appendNonEmpty groupedWords pendingGroup) (T.singleton currentChar)
  | otherwise                                     = ProgramState groupedWords (T.snoc pendingGroup currentChar)
  where
    appendNonEmpty grouped current
      | isEmpty current = grouped
      | otherwise       = grouped ++ [current]
