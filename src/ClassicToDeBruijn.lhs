Let's start with: https://markkarpov.com/tutorial/megaparsec.html

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ClassicToPrefixDeBruijn (pLVar) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

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

data LExpr
  = LVar Int
  | LAbs LExpr
  | LApp LExpr LExpr
  deriving (Eq, Ord, Show)

pLVar :: Parser LExpr
pLVar = LVar <$> integer

pLAbs :: Parser LExpr
pLAbs = LAbs <$> (lexeme (string "labs") *> pLExpr)

pLExpr :: Parser LExpr
pLExpr = pLVar <|> pLAbs

main :: IO ()
main = parseTest pLExpr "labs labs 1"
\end{code}