%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
\end{code}
\end{comment}

\begin{code}
import Control.Monad

newtype StateIO s a = StateIO {runStateIO :: s -> IO (a, s)}

prefixSumTrace :: [Int] -> IO [Int]
\end{code}
\begin{spec}
prefixSumTrace ns =
  do  (ns',_) <-  runStateIO  (traverse  (\n -> StateIO (\s -> let n' = s+n in do  print n'
                                                                                   return (n',n')))
                                         ns)
                              0
      return ns'
\end{spec}
\begin{code}
main = prefixSumTrace [2,5,3]

instance Functor (StateIO s) where fmap = liftM

instance Applicative (StateIO s) where pure = return; (<*>) = ap
\end{code}

\begin{solution}
\begin{code}
instance Monad (StateIO s) where
  return a  = StateIO (\s -> return (a, s))
  m >>= k   = StateIO (\s -> do  (a, s') <- runStateIO m s
                                 runStateIO (k a) s')
\end{code}
\end{solution}

\begin{code}
lift :: IO a -> StateIO s a
lift m = StateIO (\s -> do  a <- m
                            return (a, s))

change :: (s -> s) -> StateIO s s
change f = StateIO (\s -> let s' = f s in return (s',s'))

prefixSumTrace ns =
  do  (ns',_) <-  runStateIO  (traverse  (\n -> do  n' <- change (+n)
                                                    lift (print n')
                                                    return n')
                                         ns)
                              0
      return ns'
\end{code}
