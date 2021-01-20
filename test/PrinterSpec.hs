{-# LANGUAGE OverloadedStrings #-}

module PrinterSpec (spec) where

import Test.Hspec

import LambdaCalculus
import Printer

spec :: Spec
spec = do
  describe "ppL" $ do
    it "var" $ do
      show (ppL $ Var 1) `shouldBe` "1"

    it "abs" $ do
      show (ppL $ Abs $ Var 1) `shouldBe` "labs 1"

    it "app" $ do
      show (ppL $ App (Var 1) (Var 2)) `shouldBe` "lapp 1 2"

  describe "ppR" $ do
    it "var" $ do
      show (ppR $ Var 1) `shouldBe` "1"

    it "abs" $ do
      show (ppR $ Abs $ Var 1) `shouldBe` "1 rabs"

    it "app" $ do
      show (ppR $ App (Var 1) (Var 2)) `shouldBe` "2 1 rapp"
