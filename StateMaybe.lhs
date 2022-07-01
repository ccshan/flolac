%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
\end{code}
\end{comment}

\begin{code}
import Control.Monad

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Get | Put Expr | Div Expr Expr

eval :: Expr -> M Int
eval (Lit v)      = return v
eval (Add e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        return (v1 + v2)
eval (Mul e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        return (v1 * v2)
eval Get          = do  get
eval (Put e)      = do  v <- eval e
                        put v
                        return v
eval (Div e1 e2)  = do  v1 <- eval e1
                        v2 <- eval e2
                        divide v1 v2

instance Functor M where fmap = liftM

instance Applicative M where pure = return; (<*>) = ap

get     :: M Int
put     :: Int -> M ()
divide  :: Int -> Int -> M Int

puzzle1  =  eval (Put (Div (Put (Div Get (Lit 2))) (Lit 0)))

puzzle2  =  do  s <- get
                s' <- divide s 2
                put s'
                s'' <- divide s' 0
                put s''
\end{code}

\begin{solution}
\begin{code}
newtype M a = M {runM :: Int -> Maybe (a, Int)}

instance Monad M where
  return a  = M (\s -> Just (a, s))
  m >>= k   = M (\s -> case runM m s of  Nothing       -> Nothing
                                         Just (a, s')  -> runM (k a) s')

get         = M (\s -> Just (s, s))
put s       = M (\_ -> Just ((), s))
divide _ 0  = M (\_ -> Nothing)
divide x y  = return (x `div` y)
\end{code}
\end{solution}

\begin{solution}
\begin{spec}
newtype M a = M {runM :: Int -> (Maybe a, Int)}

instance Monad M where
  return a  = M (\s -> (Just a, s))
  m >>= k   = M (\s -> case runM m s of  (Nothing  , s') -> (Nothing, s')
                                         (Just a   , s') -> runM (k a) s')

get         = M (\s -> (Just s, s))
put s       = M (\_ -> (Just (), s))
divide _ 0  = M (\s -> (Nothing, s))
divide x y  = return (x `div` y)
\end{spec}
\end{solution}

\begin{comment}
\begin{code}
main = return ()
\end{code}
\end{comment}
