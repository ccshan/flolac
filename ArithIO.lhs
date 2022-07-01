%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
\end{code}
\end{comment}

\begin{code}
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Input | Output Expr

eval :: Expr -> IO Int
eval Input       =  getLine >>= \str -> return (read str)
eval (Output e)  =  eval e >>= \v -> print v >>= \() -> return v
\end{code}

\begin{comment}
\begin{code}
eval (Lit v)      = return v
eval (Add e1 e2)  = eval e1 >>= \v1 -> eval e2 >>= \v2 -> return (v1 + v2)
eval (Mul e1 e2)  = eval e1 >>= \v1 -> eval e2 >>= \v2 -> return (v1 * v2)

main = eval (Output (Add (Lit 2) (Lit 2)))
\end{code}
\end{comment}
