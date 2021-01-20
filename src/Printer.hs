{-# LANGUAGE FlexibleInstances #-}

module Printer where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- import LambdaCalculus

data Expr
  = Var Int
  | Abs Expr
  | App Expr Expr
  deriving (Eq, Ord, Show)

ppL :: Expr -> Doc AnsiStyle
ppL (Var a) = pretty a
ppL (Abs a) = pretty "labs" <+> ppL a
ppL (App a b) = pretty "lapp" <+> ppL a <+> ppL b

ppR :: Expr -> Doc AnsiStyle
ppR (Var a) = pretty a
ppR (Abs a) = ppR a <+> pretty "rabs"
ppR (App a b) = ppR b <+> ppR a <+> pretty "rapp"

main :: IO ()
main = do { putDoc (((pretty :: Integer -> Doc AnsiStyle) 1) <+> pretty "test")
          ; putStrLn ""
          ; (putDoc (ppL (Var 1)))
          ; putStrLn ""
          ; putDoc (ppL (Abs (Var 1)))
          ; putStrLn ""
          ; putDoc (ppL (App (Var 1) (Var 2)))
          ; putStrLn ""
          }

-- http://dev.stephendiehl.com/hask/
