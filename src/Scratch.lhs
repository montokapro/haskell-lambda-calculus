Let's start with: https://markkarpov.com/tutorial/megaparsec.html



pScheme :: Parser Text
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"

pEnvVar :: Text => Parser (Either Text Int)
pEnvVar env = many letterChar <$> Left

lookup 'b' [('a', 1), ('b', 2), ('c', 3)]
lookup 'b' [('a', 1), ('b', 2), ('c', 3)]

pEnvVar :: Eq a => [a] -> a -> Either a Int
pEnv env a
  | Just i <- result = Right (succ i)
  | Nothing <- result = Left a
  where
    result = elemIndex a env

pAbs :: Parser [String]
pAbs = do { char '|' ; space ; a <- pVar ; space ; b <- pVar ; pure [a, b] }


pAbs :: Parser [String]
pAbs = do { char '|'
          ; space
          ; a <- pVar
          ; space
          ; b <- pVar
          ; pure [a, b] }

parseTest (manyTill integer (char '.')) "0 1  . "

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ClassicToPrefixDeBruijn (pVar) where

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

integer :: Parser Integer
integer = lexeme L.decimal

variable :: Parser String
variable = (lexeme ((:) <$> letterChar <*> Text.Megaparsec.many alphaNumChar <?> "variable"))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol sc

pVar :: Parser String
pVar = variable

pAbs :: Parser [String]
pAbs = do { lexeme (char '\\')
          ; manyTill variable (lexeme (char '.'))
          ; a <- variable
          ; b <- variable
          ; pure [a, b] }
\end{code}