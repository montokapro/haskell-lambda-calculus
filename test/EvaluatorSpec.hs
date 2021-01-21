{-# LANGUAGE OverloadedStrings #-}

module EvaluatorSpec (spec) where

import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Parser
import Printer

type Parser = Parsec Void Text String

evaluate :: Parser
evaluate = pLExpr >>= \a -> pure (show $ ppL a)

spec :: Spec
spec = do
  describe "evaluate" $ do
    it "var" $ do
      parse evaluate "" "1" `shouldParse` "1"

    it "abs" $ do
      parse evaluate "" "labs 1" `shouldParse` "labs 1"

    it "app" $ do
      parse evaluate "" "lapp 1 2" `shouldParse` "lapp 1 2"

    it "app" $ do
      parse evaluate "" "lapp 1 2" `shouldParse` "lapp 1 2"

    it "irreducible" $ do
      parse evaluate "" "labs lapp 1 1" `shouldParse` "labs lapp 1 1"

    it "identity" $ do
      parse evaluate "" "lapp 1 labs 1" `shouldParse` "1"

    it "deep identity" $ do
      parse evaluate "" "lapp labs 2 labs 1" `shouldParse` "labs 2"

    it "complex" $ do
      parse evaluate "" "lapp lapp 4 3 labs labs lapp 2 1"
        `shouldParse` "labs lapp lapp 5 4 1"

    -- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf
    it "Fairouz Kamareddine" $ do
      parse evaluate "" "lapp 2 labs labs lapp 1 2"
        `shouldParse` "labs lapp 1 3"

    -- https://cs.stackexchange.com/questions/52941/lambda-calculus-reduction-examples
    it "stackexchange" $ do
      parse evaluate "" "lapp labs lapp 3 1 labs lapp 2 1"
        `shouldParse` "lapp 2 1"

    -- http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html
    it "Horwitz" $ do
      parse evaluate "" "lapp labs 1 labs labs 2"
        `shouldParse` "labs labs 1"

    -- https://en.wikipedia.org/wiki/De_Bruijn_index#Formal_definition
    it "wikipedia" $ do
      parse evaluate "" "lapp labs lapp 1 5 labs labs lapp labs lapp 3 1 lapp 2 4"
        `shouldParse` "labs lapp labs lapp labs lapp 1 7 1 lapp labs lapp 1 6 3"
