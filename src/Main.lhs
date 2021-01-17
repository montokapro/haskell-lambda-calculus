\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

import qualified Data.Text as T

import Parser
import Printer

type Parser = Parsec Void Text String

evaluate :: Parser
evaluate = pLExpr >>= \a -> pure (show $ ppL a)

main :: IO ()
main = getArgs >>= pure . T.pack . concat >>= parseTest evaluate
\end{code}

Future Work:

Consider pipes: https://hackage.haskell.org/package/pipes-4.3.14/docs/Pipes.html

Ideally this executable should be able to stream computational processing.

Demo piping between multiple executables, with some executables having reductions that others do not.


Conduits are another option:
https://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit
https://hackage.haskell.org/package/conduit
http://hackage.haskell.org/package/conduit-extra-1.3.5/docs/Data-Conduit-Attoparsec.html
