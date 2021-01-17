\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Printer (ppL, ppR) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import LambdaCalculus

ppL :: Expr -> Doc AnsiStyle
ppL (Var a) = pretty a
ppL (Abs a) = pretty "labs" <+> ppL a
ppL (App a b) = pretty "lapp" <+> ppL a <+> ppL b

ppR :: Expr -> Doc AnsiStyle
ppR (Var a) = pretty a
ppR (Abs a) = ppR a <+> pretty "rabs"
ppR (App a b) = ppR b <+> ppR a <+> pretty "rapp"
\end{code}

Special thanks to the following resource:

http://dev.stephendiehl.com/hask/
