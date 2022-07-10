%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr

eval :: (Monad m) => Expr -> m Int
prop_eval1      = eval (Add (Lit 2) (Lit 3))                == (return 5  :: [Int])
prop_eval2      = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))  == (return 6  :: [Int])
prop_evalLit v  = eval (Lit v)                              == (return v  :: [Int])
\end{code}
\end{comment}

\begin{code}
eval (Lit v)      = return v
eval (Add e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        return (v1 + v2)
eval (Mul e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        return (v1 * v2)
\end{code}

\begin{spec}
eval Get          =  State (\s -> (s, s))
eval (Put e)      =  do  v <- eval e
                         State (\_ -> (v, v))

eval (Div e1 e2)  =  do  v1 <- eval e1
                         v2 <- eval e2
                         if v2 == 0 then Nothing else return (v1 `div` v2)

eval (Amb e1 e2)  =  eval e1 ++ eval e2

eval Input        =  do  str <- getLine
                         return (read str)
eval (Output e)   =  do  v <- eval e
                         print v
                         return v
\end{spec}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
