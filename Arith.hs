{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
#ifdef SOLUTION
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(arbitrary, shrink), frequency, genericShrink)
#endif
import Test.QuickCheck (quickCheckAll)
#if STEP == 2
import qualified Data.Map as M
#endif

#if STEP == 1
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
  deriving (Eq, Show)
#endif
#if STEP == 2
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
          | Var Name | Let Name Expr Expr
  deriving (Eq, Show)
type Name = String
#endif

eval :: Expr -> Int
prop_eval1     = eval (Add (Lit 2) (Lit 3)) == 5
prop_eval2     = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3)) == 6
prop_evalLit v = eval (Lit v) == v
#if STEP == 2
prop_evalLet1  = eval (Let  "x" (Add (Lit 1) (Lit 2))
                            (Mul (Var "x") (Var "x")))
                 == 9
prop_evalLet2  = eval (Let  "y" (Add (Lit 1) (Lit 1))
                            (Let  "x" (Add (Lit 1) (Var "y"))
                                  (Mul (Var "y") (Var "x"))))
                 == 6
prop_evalLet3  = eval (Let  "x" (Add (Lit 1) (Lit 1))
                            (Let  "x" (Add (Lit 1) (Var "x"))
                                  (Mul (Var "x") (Var "x"))))
                 == 9
eval e = eval' e M.empty

eval' :: Expr -> M.Map Name Int -> Int
#endif

#ifdef SOLUTION
prop_eval_add1 e1 e2 = eval (Add e1 e2) == eval (Add e2 e1)
prop_eval_add2 v1 v2 = eval (Add (Lit v1) (Lit v2)) == v1 + v2
prop_eval_add3 e     = eval (Add e (Lit 0)) == eval e
prop_eval_mul1 e1 e2 = eval (Mul e1 e2) == eval (Mul e2 e1)
prop_eval_mul2 v1 v2 = eval (Mul (Lit v1) (Lit v2)) == v1 * v2
prop_eval_mul3 e     = eval (Mul e (Lit 1)) == eval e

deriving instance Generic Expr
instance Arbitrary Expr where
  arbitrary = frequency [ (3, Lit <$> arbitrary)
                        , (1, Add <$> arbitrary <*> arbitrary)
                        , (1, Mul <$> arbitrary <*> arbitrary) ]
                        -- not generating |Var|, |Let|
  shrink = genericShrink

#if STEP == 1
eval (Lit v)            = v
eval (Add e1 e2)        = eval e1 + eval e2
eval (Mul e1 e2)        = eval e1 * eval e2
#endif
#if STEP == 2
eval' (Lit v)       _   = v
eval' (Add e1 e2)   env = eval' e1 env + eval' e2 env
eval' (Mul e1 e2)   env = eval' e1 env * eval' e2 env
eval' (Var n)       env = env M.! n
eval' (Let n rhs e) env = eval' e (M.insert n (eval' rhs env) env)
#endif
#endif

return []
main = $quickCheckAll
