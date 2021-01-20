{-# LANGUAGE OverloadedStrings #-}

module PrinterSpec (spec) where

import Data.Text (Text)
import Data.Void
import PrettyPrinter
import Test.Hspec

import LambdaCalculus

spec :: Spec
spec = do
  describe "TODO" $ do
    it "TODO" $ do
      show (vsep ["hello", "world"]) `shouldBe` "helloworld"




-- http://dev.stephendiehl.com/hask/



class Pretty p where
  ppr :: Int -> p -> Doc AnsiStyle

instance Pretty String where
  ppr _ = pretty

instance Pretty (Doc AnsiStyle) where
  ppr _ = id

instance Pretty Expr where
  ppr _ (Var x) = pretty x
  ppr p e@(App _ _) =
    let (f, xs) = viewApp e
    in let args = sep $ map (ppr (p + 1)) xs
       in parensIf (p > 0) $ ppr p f <+> args
  ppr p e@(Lam _ _) =
    let body = ppr (p + 1) (viewBody e)
    in let vars = map (ppr 0) (viewVars e)
       in parensIf (p > 0) $ pretty '\\' <> hsep vars <+> pretty "." <+> body
