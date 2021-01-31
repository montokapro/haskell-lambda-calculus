module ZipperSpec (spec) where

import Test.Hspec
import qualified LambdaCalculus as L
import qualified Zipper as Z

spec :: Spec
spec = do
  it "zipper" $ do
    Z.zipper (L.Var 1)
      `shouldBe` Z.At (L.Var 1) Z.Top

  describe "down" $ do
    it "abs" $ do
      Z.down (Z.zipper (L.Abs (L.Var 1)))
        `shouldBe` Z.At (L.Var 1) (Z.Abs Z.Top)

    it "app" $ do
      Z.down (Z.zipper (L.App (L.Var 1) (L.Var 2)))
        `shouldBe` Z.At (L.Var 1) (Z.AppL Z.Top (L.Var 2))

  describe "up" $ do
    it "abs" $ do
      Z.up (Z.At (L.Var 1) (Z.Abs Z.Top))
        `shouldBe` Z.zipper (L.Abs (L.Var 1))

    it "left app" $ do
      Z.up (Z.At (L.Var 1) (Z.AppL Z.Top (L.Var 2)))
        `shouldBe` Z.zipper (L.App (L.Var 1) (L.Var 2))

    it "right app" $ do
      Z.up (Z.At (L.Var 2) (Z.AppR (L.Var 1) Z.Top))
        `shouldBe` Z.zipper (L.App (L.Var 1) (L.Var 2))

  describe "left" $ do
    it "app" $ do
      Z.left (Z.At (L.Var 2) (Z.AppR (L.Var 1) Z.Top))
        `shouldBe` Z.At (L.Var 1) (Z.AppL Z.Top (L.Var 2))

  describe "right" $ do
    it "app" $ do
      Z.right (Z.At (L.Var 1) (Z.AppL Z.Top (L.Var 2)))
        `shouldBe` Z.At (L.Var 2) (Z.AppR (L.Var 1) Z.Top)

  it "modify" $ do
    Z.modify (Z.At (L.Var 1) (Z.Abs Z.Top)) (\a -> L.Abs a)
      `shouldBe` Z.At (L.Abs (L.Var 1)) (Z.Abs Z.Top)
