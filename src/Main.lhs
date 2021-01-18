We use De Bruijn's notation here - not just for variable abstraction.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

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
\end{code}

We begin with a prefix representation. This should be somewhat familiar to those who understand the lambda calculus. A notable difference is that we use not only De Bruijn indicies, but De Bruijn _notation_.

This has a wonderful side effect - normally one has to extend the lambda calculus with `let` to make the lambda calculus user friendly. For example, consider the scheme code based on the lambda calculus. The following two expressions are equivalent:

(((\first . \second . (- first second)) 2) 1)

This is usually represented as:

(let [first 1] (let [second 2] (- first second)))

The De Bruijn logic approximates the let notation spatially - now the `2` is closer the argument. With psuedocode this would look like:

TODO: clean this
(app 1 app 2 \ first \ second (- first second))

\begin{code}
pLVar :: Parser LExpr
pLVar = LVar <$> integer

pLAbs :: Parser LExpr
pLAbs = LAbs <$> (lexeme (string "labs") *> pLExpr)

pLApp :: Parser LExpr
pLApp = lexeme (string "lapp") *> do { a <- pLExpr; b <- pLExpr; pure (LApp a b) }

pLExpr :: Parser LExpr
pLExpr = pLVar <|> pLAbs <|> pLApp
\end{code}

It is equally interesting to explore the postfix representation.

TODO:
This has a correlation with call-by-value.

\begin{code}
data RExpr
  = RVar Int
  | RAbs RExpr
  | RApp RExpr RExpr
  deriving (Eq, Ord, Show)

pRVar :: [RExpr] -> Parser [RExpr]
pRVar values = do { x <- integer; pure (RVar x : values) }

pRAbs :: RExpr -> [RExpr] -> Parser [RExpr]
pRAbs a values = lexeme (string "rabs") *> pure (RAbs a : values)

pRApp :: RExpr -> RExpr -> [RExpr] -> Parser [RExpr]
pRApp a b values = lexeme (string "rapp") *> pure (RApp a b : values)

pRTerm :: [RExpr] -> Parser [RExpr]
pRTerm (a : (b : values)) = pRApp a b values <|> pRAbs a (b : values) <|> pRVar (a : (b : values))
pRTerm (a : values) = pRAbs a values <|> pRVar (a : values)
pRTerm values = pRVar values

pRExpr :: Parser [RExpr]
pRExpr = pRTerm empty
\end{code}

\begin{code}
main :: IO ()
main = do { parseTest pLExpr "lapp labs labs 1 2"
          ; parseTest (pRTerm []) "1"
          ; parseTest (pRTerm [RVar 1]) "rabs"
          ; parseTest (pRTerm [RVar 1]) "2"
          ; parseTest (pRTerm [RVar 1, RVar 2]) "rapp"
          }
\end{code}

Special thanks to the following resources:

https://markkarpov.com/tutorial/megaparsec.html
