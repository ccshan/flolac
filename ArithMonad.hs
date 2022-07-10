%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
\end{code}
\end{comment}

\begin{code}
import Control.Monad

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr

eval :: (Monad m) => Expr -> m Int
prop_eval1      = eval (Add (Lit 2) (Lit 3))                == (return 5  :: [Int])
prop_eval2      = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))  == (return 6  :: [Int])
prop_evalLit v  = eval (Lit v)                              == (return v  :: [Int])

eval (Lit v)      = return v
eval (Add e1 e2)  = eval e1 >>= \v1 -> eval e2 >>= \v2 -> return (v1 + v2)
eval (Mul e1 e2)  = eval e1 >>= \v1 -> eval e2 >>= \v2 -> return (v1 * v2)

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where fmap = liftM

instance Applicative (State s) where pure = return; (<*>) = ap

instance Monad (State s) where
  return a  = State (\s ->  (a, s))
  m >>= k   = State (\s ->  let (a, s') = runState m s
                            in runState (k a) s')
\end{code}

\begin{spec}
eval Get          =  State (\s -> (s, s))
eval (Put e)      =  eval e >>= \v -> State (\_ -> (v, v))

eval (Div e1 e2)  =  eval e1 >>= \v1 -> eval e2 >>= \v2 ->
                     if v2 == 0 then Nothing else return (v1 `div` v2)

eval (Amb e1 e2)  =  eval e1 ++ eval e2
\end{spec}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
