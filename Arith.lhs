%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll, Arbitrary(arbitrary, shrink), frequency, genericShrink)
import GHC.Generics (Generic)
import qualified Data.Map as M
\end{code}
\end{comment}

\begin{nonchallenge}
\begin{spec}
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
\end{spec}

\begin{code}
eval :: Expr -> Int
prop_eval1      = eval (Add (Lit 2) (Lit 3)) == 5
prop_eval2      = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3)) == 6
prop_evalLit v  = eval (Lit v) == v
\end{code}
\end{nonchallenge}

\begin{challenge}
\begin{code}
data Expr  =  Lit Int | Add Expr Expr | Mul Expr Expr
           |  Var Name | Let Name Expr Expr
  deriving (Eq, Show)
type Name  =  String

prop_evalLet1  =  eval (Let  "x" (Add (Lit 1) (Lit 2))
                             (Mul (Var "x") (Var "x")))
                    == 9
prop_evalLet2  =  eval (Let  "y" (Add (Lit 1) (Lit 1))
                             (Let  "x" (Add (Lit 1) (Var "y"))
                                   (Mul (Var "y") (Var "x"))))
                    == 6
prop_evalLet3  =  eval (Let  "x" (Add (Lit 1) (Lit 1))
                             (Let  "x" (Add (Lit 1) (Var "x"))
                                   (Mul (Var "x") (Var "x"))))
                    == 9
\end{code}
\end{challenge}

\begin{solution}
\begin{code}
prop_eval_add1 e1 e2  = eval (Add e1 e2) == eval (Add e2 e1)
prop_eval_add2 v1 v2  = eval (Add (Lit v1) (Lit v2)) == v1 + v2
prop_eval_add3 e      = eval (Add e (Lit 0)) == eval e
prop_eval_mul1 e1 e2  = eval (Mul e1 e2) == eval (Mul e2 e1)
prop_eval_mul2 v1 v2  = eval (Mul (Lit v1) (Lit v2)) == v1 * v2
prop_eval_mul3 e      = eval (Mul e (Lit 1)) == eval e

deriving instance Generic Expr
instance Arbitrary Expr where
  arbitrary = frequency  [  (3, Lit <$> arbitrary)
                         ,  (1, Add <$> arbitrary <*> arbitrary)
                         ,  (1, Mul <$> arbitrary <*> arbitrary) ]
                            -- not generating |Var|, |Let|
  shrink = genericShrink

eval e = eval' e M.empty

eval' :: Expr -> M.Map Name Int -> Int
eval' (Lit v)        _    = v
eval' (Add e1 e2)    env  = eval' e1 env + eval' e2 env
eval' (Mul e1 e2)    env  = eval' e1 env * eval' e2 env
eval' (Var n)        env  = env M.! n
eval' (Let n rhs e)  env  = eval' e (M.insert n (eval' rhs env) env)
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
