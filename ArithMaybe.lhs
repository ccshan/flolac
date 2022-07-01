%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
\end{code}
\end{comment}

\begin{code}
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Div Expr Expr
type State = Int

eval :: Expr -> Maybe Int
prop_eval1      = eval (Add (Lit 2) (Lit 3))                                 == Just 5
prop_eval2      = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))                   == Just 6
prop_evalLit v  = eval (Lit v)                                               == Just v
prop_evalDiv1   = eval (Div (Lit 6) (Lit 2))                                 == Just 3
prop_evalDiv2   = eval (Mul (Div (Lit 3) (Add (Lit 1) (Lit (-1)))) (Lit 2))  == Nothing
\end{code}

\begin{solution}
\begin{code}
eval (Lit v)      =  Just v
eval (Add e1 e2)  =  case eval e1 of
                       Nothing  ->  Nothing
                       Just v1  ->  case eval e2 of
                                      Nothing  ->  Nothing
                                      Just v2  ->  Just (v1 + v2)
eval (Mul e1 e2)  =  case eval e1 of
                       Nothing  ->  Nothing
                       Just v1  ->  case eval e2 of
                                      Nothing  ->  Nothing
                                      Just v2  ->  Just (v1 * v2)
eval (Div e1 e2)  =  case eval e1 of
                       Nothing  ->  Nothing
                       Just v1  ->  case eval e2 of
                                      Nothing  ->  Nothing
                                      Just 0   ->  Nothing
                                      Just v2  ->  Just (v1 `div` v2)
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
