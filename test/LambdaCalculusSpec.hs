module LambdaCalculusSpec (spec) where

import Test.Hspec
import LambdaCalculus

spec :: Spec
spec = do
  describe "evaluate" $ do
    it "irreducible" $ do
      evaluate (Abs (App (Var 1) (Var 1))) `shouldBe` Abs (App (Var 1) (Var 1))

    it "identity" $ do
      evaluate (App (Var 1) (Abs (Var 1))) `shouldBe` Var 1

    it "deep identity" $ do
      evaluate (App (Abs (Var 2)) (Abs (Var 1))) `shouldBe` Abs (Var 2)

    it "complex" $ do
      evaluate (App (App (Var 4) (Var 3)) (Abs (Abs (App (Var 2) (Var 1))))) `shouldBe` Abs (App (App (Var 5) (Var 4)) (Var 1))

    -- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf
    it "Fairouz Kamareddine" $ do
      evaluate (App (Var 2) (Abs (Abs (App (Var 1) (Var 2))))) `shouldBe` Abs (App (Var 1) (Var 3))

--       (run-tests
--  (test-suite
--   "eval-all"
--   (test-case "irreducible"
--     (check-equal?
--      (eval-all '(abs (app 1 1)))
--      '(abs (app 1 1))))
--   (test-case "identity"
--     (check-equal?
--      (eval-all '(app (abs 1) 1))
--      '1))
--   (test-case "deep identity"
--     (check-equal?
--      (eval-by-value '(app (abs 1) (abs 2)))
--      '(abs 2)))
--   (test-case "complex"
--     (check-equal?
--      (eval-by-value '(app (abs (abs (app 1 2))) (app 3 4)))
--      '(abs (app 1 (app 4 5)))))
--   ; https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf
--   (test-case "Fairouz Kamareddine"
--     (check-equal?
--      (eval-by-name '(app (abs (abs (app 2 1))) 2))
--      '(abs (app 3 1))))
--   ; https://cs.stackexchange.com/questions/52941/lambda-calculus-reduction-examples
--   (test-case "stackexchange"
--     (check-equal?
--      (eval-all '(app (abs (app 1 2)) (abs (app 1 3))))
--      '(app 1 2)))
--   ; http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html
--   (test-case "wisc"
--     (check-equal?
--      (eval-all '(app (abs (abs 2)) (abs 1)))
--      '(abs (abs 1))))
--   ; https://en.wikipedia.org/wiki/De_Bruijn_index#Formal_definition
--   (test-case "wikipedia"
--     (check-equal?
--      ; (λ λ 4 2 (λ 1 3)) (λ 5 1)
--      (eval-all '(app (abs (abs (app (app 4 2) (abs (app 1 3))))) (abs (app 5 1))))
--      ; λ 3 (λ 6 1) (λ 1 (λ 7 1))
--      '(abs (app (app 3 (abs (app 6 1))) (abs (app 1 (abs (app 7 1))))))))))

-- (run-tests
--  (test-suite
--   "eval-by-name-fixpoint"
--   (test-case "step"
--     (check-equal?
--      ((eval-by-name-fixpoint (λ (a) `(eval ,a))) '(app (abs 1) 2))
--      '(eval 2)))))

-- (run-tests
--  (test-suite
--   "eval-by-value-fixpoint"
--   (test-case "step"
--     (check-equal?
--      ((eval-by-value-fixpoint (λ (a) `(eval ,a))) '(app (abs 1) 2))
--      `(shift (eval 2) 0 1)))))

-- (run-tests
--  (test-suite
--   "eval-by-name"
--   (test-case "irreducible"
--     (check-equal?
--      (eval-by-name '(abs (app 1 1)))
--      '(abs (app 1 1))))
--   (test-case "identity"
--     (check-equal?
--      (eval-by-name '(app (abs 1) 1))
--      '1))
--   (test-case "deep identity"
--     (check-equal?
--      (eval-by-name '(app (abs 1) (abs 2)))
--      '(abs 2)))
--   (test-case "complex"
--     (check-equal?
--      (eval-by-name '(app (abs (abs (app 1 2))) (app 3 4)))
--      '(abs (app 1 (app 4 5)))))
--   ;; (test-case "evaluates omega once"
--   ;;   (check-equal?
--   ;;    (eval-by-name '(app (abs (app 1 1)) (abs (app 1 1))))
--   ;;    '(app (abs (app 1 1)) (abs (app 1 1)))))
--   ;; (test-case "does not evaluate omega"
--   ;;   (check-equal?
--   ;;    (eval-by-name '(app (abs (abs 1)) (app (abs (app 1 1)) (abs (app 1 1)))))
--   ;;    '(abs 1)))
--   ; https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf
--   (test-case "Fairouz Kamareddine"
--     (check-equal?
--      (eval-by-name '(app (abs (abs (app 2 1))) 2))
--      '(abs (app 3 1))))
--   ; https://cs.stackexchange.com/questions/52941/lambda-calculus-reduction-examples
--   (test-case "stackexchange"
--     (check-equal?
--      (eval-by-name '(app (abs (app 1 2)) (abs (app 1 3))))
--      '(app 1 2)))
--   ; http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html
--   (test-case "wisc"
--     (check-equal?
--      (eval-by-name '(app (abs (abs 2)) (abs 1)))
--      '(abs (abs 1))))
--   ; https://en.wikipedia.org/wiki/De_Bruijn_index#Formal_definition
--   (test-case "wikipedia"
--     (check-equal?
--      ; (λ λ 4 2 (λ 1 3)) (λ 5 1)
--      (eval-by-name '(app (abs (abs (app (app 4 2) (abs (app 1 3))))) (abs (app 5 1))))
--      ; λ 3 (λ 6 1) (λ 1 (λ 7 1))
--      '(abs (app (app 3 (abs (app 6 1))) (abs (app 1 (abs (app 7 1))))))))))

-- (run-tests
--  (test-suite
--   "eval-by-value"
--   (test-case "irreducible"
--     (check-equal?
--      (eval-by-value '(abs (app 1 1)))
--      '(abs (app 1 1))))
--   (test-case "identity"
--     (check-equal?
--      (eval-by-value '(app (abs 1) 1))
--      '1))
--   (test-case "deep identity"
--     (check-equal?
--      (eval-by-value '(app (abs 1) (abs 2)))
--      '(abs 2)))
--   (test-case "complex"
--     (check-equal?
--      (eval-by-value '(app (abs (abs (app 1 2))) (app 3 4)))
--      '(abs (app 1 (app 4 5)))))
--   ;; (test-case "evaluates omega infinitely"
--   ;;   (check-equal?
--   ;;    (eval-by-name '(app (abs (app 1 1)) (abs (app 1 1))))
--   ;;    '(app (abs (app 1 1)) (abs (app 1 1)))))
--   ;; (test-case "evaluate omega"
--   ;;   (check-equal?
--   ;;    (eval-by-name '(app (abs (abs 1)) (app (abs (app 1 1)) (abs (app 1 1)))))
--   ;;    '(abs 1)))
--   ; https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.296.2485&rep=rep1&type=pdf
--   (test-case "Fairouz Kamareddine"
--     (check-equal?
--      (eval-by-value '(app (abs (abs (app 2 1))) 2))
--      '(abs (app 3 1))))
--   ; https://cs.stackexchange.com/questions/52941/lambda-calculus-reduction-examples
--   (test-case "stackexchange"
--     (check-equal?
--      (eval-by-value '(app (abs (app 1 2)) (abs (app 1 3))))
--      '(app (abs (app 1 3)) 1)))
--   ; http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html
--   (test-case "wisc"
--     (check-equal?
--      (eval-by-value '(app (abs (abs 2)) (abs 1)))
--      '(abs (abs 1))))
--   ; https://en.wikipedia.org/wiki/De_Bruijn_index#Formal_definition
--   (test-case "wikipedia"
--     (check-equal?
--      ; (λ λ 4 2 (λ 1 3)) (λ 5 1)
--      (eval-by-value '(app (abs (abs (app (app 4 2) (abs (app 1 3))))) (abs (app 5 1))))
--      ; λ 3 (λ 6 1) (λ 1 (λ 7 1))
--      '(abs (app (app 3 (abs (app 6 1))) (abs (app 1 (abs (app 7 1))))))))))
