{-# LANGUAGE OverloadedStrings #-}

module TypeSpec (spec) where

import Test.Hspec
import LambdaCalculus

-- unify :: Mono -> Mono -> Poly -> Poly
unify :: Expr -> Expr -> Expr -> Expr
unify a b = unify' [(a, b)] id

-- unify' :: [(Mono, Mono)] -> (Poly -> Poly) -> Poly -> Poly
unify' :: [(Expr, Expr)] -> (Expr -> Expr) -> Expr -> Expr
unify' [] r = r
unify' ((a, b) : ag) r = unify' ag' (r' . r)
  where
    (ag', r') = subMany ag a b

-- subMany :: [(Mono, Mono)] -> Mono -> Mono -> ([(Mono, Mono)], Poly -> Poly)
subMany :: [(Expr, Expr)] -> Expr -> Expr -> ([(Expr, Expr)], Expr -> Expr)
subMany ag (App a1 a2) (App b1 b2) = ((a1, b1) : (a2, b2) : ag, id)
subMany ag (Var a) tb  = subManyVar ag tb a
subMany ag ta (Var b) = subManyVar ag ta b
subMany ag a b = error $ "Types " ++ (show a) ++ " and " ++ (show b) ++ " could not be unified"

-- subManyVar :: [(Mono, Mono)] -> Mono -> Int -> ([(Mono, Mono)], Poly -> Poly)
subManyVar :: [(Expr, Expr)] -> Expr -> Int -> ([(Expr, Expr)], Expr -> Expr)
subManyVar ag t i = (map (\(a, b) -> (substitute t a i, substitute t b i)) ag, s)
  where
    s x = substitute t x i

-- old sub :: i -> Mono -> Poly -> Poly
-- new sub :: Poly -> Mono -> i -> Poly

-- data Mono = Var Int | App Mono Mono
-- data Poly = Abs Int Poly | Mono

shouldUnify :: HasCallStack => Expr -> Expr -> Expectation
a `shouldUnify` b = fa `shouldBe` fb
  where
    f = unify a b
    fa = f a
    fb = f b

-- Backport from https://hackage.haskell.org/package/relude-0.7.0.0/docs/src/Relude.List.html#%21%21%3F
-- License: MIT
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

-- -- Because App is in the opposite order from the classical form, this is significantly easier
infer :: [Expr] -> Expr -> Expr
infer env a@(Var b) = case (env !!? (pred b)) of
  Just c -> c
  Nothing -> a
infer env (App a b) = App (infer env a) (infer env b)
infer env (Abs a) = Abs (infer ((Var 1) : env) a)

spec :: Spec
spec = do
  -- https://blog.lahteenmaki.net/combinator-birds.html
  describe "infer" $ do
    it "I" $ do
      infer [] (Abs (Var 1))
        `shouldBe` Abs (Var 1)

    -- it "K" $ do
    --   infer [] (Abs (Abs (Var 1)))
    --     `shouldBe` (Abs (Abs (App (Var 2) (App (Var 1) (Var 2)))))

    -- it "T" $ do
    --   infer Abs $ Abs $ App (Var 1) (Var 2)
    --     `shouldBe` Abs $ Abs $ App (Var 2) (App (App (Var 2) (Var 1)) (Var 1))

    -- it "S" $ do
    --   infer Abs $ Abs $ Abs $ App (App (Var 3) (Var 1)) (App (Var 2) (Var 1))
    --     `shouldBe` Abs $ Abs $ Abs $
    --     (App
    --      (App
    --       (App
    --        (App (Var 3) (App (Var 2) (Var 1)))
    --        (App (Var 3) (Var 2)))
    --       (Var 3))
    --      (Var 1))

    -- \a \b . b a : \a \b . a -> (a -> b) -> b

  describe "unify" $ do
    it "free vars" $ do
      (Var 1) `shouldUnify` (Var 1)

    it "apps with free vars" $ do
      App (Var 1) (Var 2) `shouldUnify` App (Var 1) (Var 2)

    it "apps with different free vars" $ do
      App (Var 1) (Var 2) `shouldUnify` App (Var 3) (Var 4)

    it "beta reduction" $ do
      App (Var 1) (Var 1) `shouldUnify` App (App (Var 2) (Var 2)) (Var 3)

    it "beta reduction reversed" $ do
      App (App (Var 2) (Var 2)) (Var 3) `shouldUnify` App (Var 1) (Var 1)
