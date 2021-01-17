{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import LambdaCalculus
import Parser

type Parser = Parsec Void Text

spec :: Spec
spec = do
  describe "pLExpr" $ do
    it "var" $ do
      parse (pLExpr :: Parser Expr) "" "1" `shouldParse` Var 1

    it "abs" $ do
      parse (pLExpr :: Parser Expr) "" "labs 1" `shouldParse` Abs (Var 1)

    it "app" $ do
      parse (pLExpr :: Parser Expr) "" "lapp 1 2" `shouldParse` App (Var 1) (Var 2)

