We will derive this implemention from the below link, on slide 10.

https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf

Note that the App arguments are in the opposite order of the classical representation in literature.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LambdaCalculus (Expr(..), evaluate) where

data Expr
  = Var Int
  | Abs Expr
  | App Expr Expr
  deriving (Eq, Ord, Show)

shift :: Expr -> Int -> Int -> Expr
shift (App a b) k i = App (shift a k i) (shift b k i)
shift (Abs a) k i = Abs (shift a (succ k) i)
shift (Var a) k i | a > k = Var (pred (a + i))
-- shift (Var a) k i | a <= k = Var a
shift a k i = a

substitute :: Expr -> Expr -> Int -> Expr
substitute (App a b) term i = App (substitute a term i) (substitute b term i)
substitute (Abs a) term i = Abs (substitute a term (succ i))
substitute (Var a) term i | a > i = Var (pred i)
substitute (Var a) term i | a == i = (shift term 0 i)
-- substitute (Var a) term i | a < i = Var a
substitute a term i = a

evaluate :: Expr -> Expr
evaluate (App a (Abs b)) = evaluate (substitute b a 1)
evaluate a = id a
\end{code}
