\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Text.Megaparsec

import Parser

main :: IO ()
main = do { parseTest pLExpr "lapp labs 1 labs 3"
          -- ; parseTest pLExpr "lapp 2 labs 1"
          -- ; parseTest pLExpr "lapp 2 labs labs 2"
          -- ; parseTest pLExpr "lapp 2 labs labs 1"
          -- ; parseTest pLExpr "lapp labs 1 labs lapp 4 2"
          -- ; parseTest (pRTerm []) "1"
          -- ; parseTest (pRTerm [Var 1]) "rabs"
          -- ; parseTest (pRTerm [Var 1]) "2"
          -- ; parseTest (pRTerm [Var 1, Var 2]) "rapp"
          }
\end{code}
