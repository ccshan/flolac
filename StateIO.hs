{-# OPTIONS -W #-}
#if STEP <= 2
import Control.Monad (liftM, ap)
#endif
#if STEP == 3
import Control.Monad.Trans.State (StateT(runStateT), modify, get)
import Control.Monad.Trans.Class (lift)
#endif

main :: IO ()
main = do putStrLn "prefixSumTrace [2,5,3]"
          result <- prefixSumTrace [2,5,3]
          putStr "result = "
          print result
          putStrLn "prefixWeightedSumTrace [2,5,3]"
          result <- prefixWeightedSumTrace [2,5,3]
          putStr "result = "
          print result

prefixSumTrace :: [Int] -> IO [Int]
prefixSumTrace ns =
#if STEP == 1
  do (ns',_) <- runStateIO (traverse (\n -> StateIO (\s -> let n' = s + n in
                                                           do print n'
                                                              return (n',n')))
                                     ns)
                           0
     return ns'
#endif
#if STEP >= 2
  do (ns',_) <- runStateIO (traverse (\n -> do n' <- change (+ n)
                                               lift (print n')
                                               return n')
                                     ns)
                           0
     return ns'
#endif

prefixWeightedSumTrace :: [Int] -> IO [Int]
prefixWeightedSumTrace ns =
#if STEP == 1
  do (ns',_) <- runStateIO (traverse (\n -> StateIO (\s -> do str <- getLine
                                                              let n' = s + n * read str
                                                              print n'
                                                              return (n',n')))
                                     ns)
                           0
     return ns'
#endif
#if STEP >= 2
  do (ns',_) <- runStateIO (traverse (\n -> do str <- lift getLine
                                               n' <- change (+ n * read str)
                                               lift (print n')
                                               return n')
                                     ns)
                           0
     return ns'
#endif

#if STEP <= 2
newtype StateIO s a = StateIO {runStateIO :: s -> IO (a, s)}

instance Functor (StateIO s) where fmap = liftM

instance Applicative (StateIO s) where pure = return; (<*>) = ap

instance Monad (StateIO s) where
#ifdef SOLUTION
  return a = StateIO (\s -> return (a, s))
  m >>= k  = StateIO (\s -> do (a, s') <- runStateIO m s
                               runStateIO (k a) s')
#endif
#endif

#if STEP == 2
#ifdef SOLUTION
lift :: IO a -> StateIO s a
lift m = StateIO (\s -> do a <- m
                           return (a, s))

change :: (s -> s) -> StateIO s s
change f = StateIO (\s -> let s' = f s in return (s',s'))
#endif
#endif

#if STEP == 3
type StateIO s = StateT s IO

runStateIO :: StateIO s a -> s -> IO (a, s)
runStateIO = runStateT

change :: (s -> s) -> StateIO s s
#ifdef SOLUTION
change f = modify f >> get
#endif
#endif
