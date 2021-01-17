module LambdaCalculusSpec (spec) where

import Test.Hspec
import LambdaCalculus

spec :: Spec
spec = do
  describe "evaluate" $ do
    it "irreducible" $ do
      evaluate (Abs (App (Var 1) (Var 1)))
        `shouldBe` Abs (App (Var 1) (Var 1))

    it "identity" $ do
      evaluate (App (Var 1) (Abs (Var 1)))
        `shouldBe` Var 1

    it "deep identity" $ do
      evaluate (App (Abs (Var 2)) (Abs (Var 1)))
        `shouldBe` Abs (Var 2)

    it "complex" $ do
      evaluate (App (App (Var 4) (Var 3)) (Abs (Abs (App (Var 2) (Var 1)))))
        `shouldBe` Abs (App (App (Var 5) (Var 4)) (Var 1))

    -- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf
    it "Fairouz Kamareddine" $ do
      evaluate (App (Var 2) (Abs (Abs (App (Var 1) (Var 2)))))
        `shouldBe` Abs (App (Var 1) (Var 3))

    -- https://cs.stackexchange.com/questions/52941/lambda-calculus-reduction-examples
    it "stackexchange" $ do
      evaluate (App (Abs (App (Var 3) (Var 1))) (Abs (App (Var 2) (Var 1))))
        `shouldBe` App (Var 2) (Var 1)

    -- http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html
    it "Horwitz" $ do
      evaluate (App (Abs (Var 1)) (Abs (Abs (Var 2))))
        `shouldBe` Abs (Abs (Var 1))

    -- https://en.wikipedia.org/wiki/De_Bruijn_index#Formal_definition
    it "wikipedia" $ do
      evaluate (App (Abs (App (Var 1) (Var 5))) (Abs (Abs (App (Abs (App (Var 3) (Var 1))) (App (Var 2) (Var 4))))))
        `shouldBe` Abs (App (Abs (App (Abs (App (Var 1) (Var 7))) (Var 1))) (App (Abs (App (Var 1) (Var 6))) (Var 3)))
