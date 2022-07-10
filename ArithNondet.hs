%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
\end{code}
\end{comment}

\begin{code}
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Amb Expr Expr

eval :: Expr -> [Int]
prop_eval1      = eval (Add (Lit 2) (Lit 3))                                 == [5]
prop_eval2      = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))                   == [6]
prop_evalLit v  = eval (Lit v)                                               == [v]
prop_evalDiv    = eval (Mul (Add (Lit 1) (Amb (Lit 0) (Lit 1))) (Lit 3))     == [3,6]
\end{code}

\begin{solution}
\begin{code}
eval (Lit v)      =  [v]
eval (Add e1 e2)  =  concatMap  (\v1 -> map  (\v2 -> v1+v2)
                                             (eval e2))
                                (eval e1)
eval (Mul e1 e2)  =  concatMap  (\v1 -> map  (\v2 -> v1*v2)
                                             (eval e2))
                                (eval e1)
eval (Amb e1 e2)  =  eval e1 ++ eval e2
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
