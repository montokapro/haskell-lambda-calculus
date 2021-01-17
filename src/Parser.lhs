\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser (pLExpr, integer) where

import Control.Applicative
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import LambdaCalculus
\end{code}

\begin{code}
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lookAhead (satisfy (/= '0')) *> lexeme L.decimal
\end{code}

We begin with a prefix representation. This should be somewhat familiar to those who understand the lambda calculus. A notable difference is that we use not only De Bruijn indicies, but De Bruijn notation. Here, the arguments of App are reversed.

\begin{code}
pLVar :: Parser Expr
pLVar = Var <$> integer

pLAbs :: Parser Expr
pLAbs = lexeme (string "labs") *> (Abs <$> pLExpr)

pLApp :: Parser Expr
pLApp = evaluate <$> (lexeme (string "lapp") *> (App <$> pLExpr <*> pLExpr))

pLExpr :: Parser Expr
pLExpr = pLVar <|> pLAbs <|> pLApp
\end{code}

Special thanks to the following resource:

https://markkarpov.com/tutorial/megaparsec.html
