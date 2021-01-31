This code is heavily inspired by:
https://github.com/srijs/haskell-lambda-term-zipper/blob/master/src/Data/Lambda/Term/Zipper.hs

\begin{code}
module Zipper (Context(..), Location(..), zipper, down, up, left, right, modify) where

import qualified LambdaCalculus as L

data Context
  = Top
  | Abs Context
  | AppL Context L.Expr
  | AppR L.Expr Context
  deriving (Eq, Show)

data Location = At { focus :: L.Expr, with :: Context } deriving (Eq, Show)

zipper :: L.Expr -> Location
zipper expr = At expr Top

down :: Location -> Location
down (At (L.Abs a) context) = At a (Abs context)
down (At (L.App a b) context) = At a (AppL context b)
down location = location

up :: Location -> Location
up (At a (Abs context)) = At (L.Abs a) context
up (At a (AppL context b)) = At (L.App a b) context
up (At a (AppR b context)) = At (L.App b a) context
up location = location

left :: Location -> Location
left (At a (AppR b context)) = At b (AppL context a)
left location = location

right :: Location -> Location
right (At a (AppL context b)) = At b (AppR a context)
right location = location

modify :: Location -> (L.Expr -> L.Expr) -> Location
modify (At a b) f = At (f a) b
\end{code}
