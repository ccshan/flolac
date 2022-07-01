%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
\end{code}
\end{comment}

\begin{code}
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Get | Put Expr
type State = Int

eval :: Expr -> State -> (Int, State)
prop_eval1      s  = eval (Add (Lit 2) (Lit 3))                s   == (5, s)
prop_eval2      s  = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))  s   == (6, s)
prop_evalLit v  s  = eval (Lit v)                              s   == (v, s)
prop_evalState1    = eval Get                                  50  == (50, 50)
prop_evalState2    = eval (Add Get (Lit 1))                    50  == (51, 50)
prop_evalState3    = eval (Put (Lit 51))                       50  == (51, 51)
prop_evalState4    = eval (Put (Add Get (Lit 1)))              50  == (51, 51)
prop_evalState5    = eval (Add Get (Put (Add Get (Lit 1))))    50  == (101, 51)
prop_evalState6    = eval (Add (Put (Add Get (Lit 1))) Get)    50  == (102, 51)
prop_evalPutGet s  = eval (Put Get)                            s   == eval Get s
\end{code}

\begin{solution}
\begin{code}
eval (Lit v)      s  =  (v, s)
eval (Add e1 e2)  s  =  let  (v1, s1) = eval e1 s
                             (v2, s2) = eval e2 s1
                        in (v1 + v2, s2)
eval (Mul e1 e2)  s  =  let  (v1, s1) = eval e1 s
                             (v2, s2) = eval e2 s1
                        in (v1 * v2, s2)
eval Get          s  =  (s, s)
eval (Put e)      s  =  let  (v, _) = eval e s
                        in (v, v)
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
