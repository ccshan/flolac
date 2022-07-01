%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
\end{code}
\end{comment}

\begin{solution}
\begin{spec}
import Control.Monad.Trans.State (runState, state)
\end{spec}
\end{solution}

\begin{code}
import Control.Monad

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Get | Put Expr | Amb Expr Expr

eval :: Expr -> M Int
eval (Lit v)      = return v
eval (Add e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        return (v1 + v2)
eval (Mul e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        return (v1 * v2)
eval Get          = get
eval (Put e)      = do  v <- eval e
                        put v
                        return v
eval (Amb e1 e2)  = amb [eval e1, eval e2]

instance Functor M where fmap = liftM

instance Applicative M where pure = return; (<*>) = ap

get     :: M Int
put     :: Int -> M ()
amb     :: [M a] -> M a

puzzle1  =  eval (Amb  (Put (Add Get (Lit 1)))
                       (Put (Add Get (Lit 2))))

puzzle2  =  amb  [  do   s <- get
                         put (s+1)
                 ,  do   s <- get
                         put (s+2)]
\end{code}

\begin{solution}
\begin{code}
newtype M a = M {runM :: Int -> [(a, Int)]}

instance Monad M where
  return a  = M (\s -> [(a, s)])
  m >>= k   = M (\s -> concatMap  (\(a, s') -> runM (k a) s')
                                  (runM m s))

get         = M (\s -> [(s, s)])
put s       = M (\_ -> [((), s)])
amb ms      = M (\s -> concatMap (\m -> runM m s) ms)
\end{code}
\end{solution}

\begin{solution}
\begin{spec}
newtype M a = M {runM :: Int -> ([a], Int)}

instance Monad M where
  return a  = M (\s -> ([a], s))
  m >>= k   = M (runState (do  as <- state (runM m)
                               bss <- traverse (\a -> state (runM (k a))) as
                               return (concat bss)))

get         = M (\s -> ([s], s))
put s       = M (\_ -> ([()], s))
amb ms      = M (runState (do  ass <- traverse (\m -> state (runM m)) ms
                               return (concat ass)))
\end{spec}
\end{solution}

\begin{comment}
\begin{code}
main = return ()
\end{code}
\end{comment}
