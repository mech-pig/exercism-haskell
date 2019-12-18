{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Acronym (abbreviate, defaultConfiguration, evaluateChar, ProgramState(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = do
  describe "evaluateChar" $ do

    it "empty state, lowercase char" $
      evaluateChar defaultConfiguration (ProgramState [] "") 's' `shouldBe` (ProgramState [] "s")

    it "empty state, uppercase char" $
      evaluateChar defaultConfiguration (ProgramState [] "") 'S' `shouldBe` (ProgramState [] "S")

    it "empty state, non alphanum char" $
      evaluateChar defaultConfiguration (ProgramState [] "") '%' `shouldBe` (ProgramState [] "")

    it "empty state, separator char" $
      evaluateChar defaultConfiguration (ProgramState [] "") '-' `shouldBe` (ProgramState [] "")

    it "lowercase pending, lowercase char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "c") 't' `shouldBe` (ProgramState ["group"] "ct")

    it "lowercase pending, uppercase char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "c") 'T' `shouldBe` (ProgramState ["group", "c"] "T")

    it "lowercase pending, non alphanum char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "c") '%' `shouldBe` (ProgramState ["group"] "c")

    it "lowercase pending, separator char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "c") '-' `shouldBe` (ProgramState ["group", "c"] "")

    it "uppercase pending, lowercase char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "C") 't' `shouldBe` (ProgramState ["group"] "Ct")

    it "uppercase pending, uppercase char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "C") 'T' `shouldBe` (ProgramState ["group"] "CT")

    it "uppercase pending, non alphanum char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "C") '%' `shouldBe` (ProgramState ["group"] "C")

    it "uppercase pending, separator char" $
      evaluateChar defaultConfiguration (ProgramState ["group"] "C") '-' `shouldBe` (ProgramState ["group", "C"] "")

  describe "abbreviate" $ do
    let
      test Case {..} = it description $ abbreviate (fromString input) `shouldBe` (fromString expected)
      cases = [ Case { description = "basic"
                     , input       = "Portable Network Graphics"
                     , expected    = "PNG"
                     }
              , Case { description = "lowercase words"
                     , input       = "Ruby on Rails"
                     , expected    = "ROR"
                     }
              -- Although this case was removed in specification 1.1.0,
              -- the Haskell track has chosen to keep it,
              -- since it makes the problem more interesting.
              , Case { description = "camelcase"
                     , input       = "HyperText Markup Language"
                     , expected    = "HTML"
                     }
              , Case { description = "punctuation"
                     , input       = "First In, First Out"
                     , expected    = "FIFO"
                     }
              , Case { description = "all caps word"
                     , input       = "GNU Image Manipulation Program"
                     , expected    = "GIMP"
                     }
              , Case { description = "punctuation without whitespace"
                     , input       = "Complementary metal-oxide semiconductor"
                     , expected    = "CMOS"
                     }
              , Case { description = "very long abbreviation"
                     , input       = "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
                     , expected    = "ROTFLSHTMDCOALM"
                     }
              , Case { description = "consecutive delimiters"
                     , input       = "Something - I made up from thin air"
                     , expected    = "SIMUFTA"
                     }
              , Case { description = "apostrophes"
                     , input       = "Halley's Comet"
                     , expected    = "HC"
                     }
              , Case { description = "underscore emphasis"
                     , input       = "The Road _Not_ Taken"
                     , expected    = "TRNT"
                     }
              ]
    for_ cases test




data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }
