\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

main :: IO ()
main = getArgs >>= pure . concat >>= putStrLn
\end{code}

Future Work:

Consider pipes: https://hackage.haskell.org/package/pipes-4.3.14/docs/Pipes.html

Ideally this executable should be able to stream computational processing.

Demo piping between multiple executables, with some executables having reductions that others do not.
