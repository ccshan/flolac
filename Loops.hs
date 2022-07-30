{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
#if STEP == 2
import Control.Monad.Trans.State
#endif

#if STEP == 1
#ifdef SOLUTION
forever :: (Monad m) => m a -> m b
#endif
forever action = action >> forever action

#ifdef SOLUTION
echo :: IO b
echo = forever (getLine >>= putStrLn)
#endif

#ifdef SOLUTION
iterateM_ :: (Monad m) => (a -> m a) -> (a -> m b)
#endif
iterateM_ f x = f x >>= iterateM_ f

#ifdef SOLUTION
register :: IO b
register = iterateM_ (\sum -> do str <- getLine
                                 let sum' = sum + read str
                                 print sum'
                                 return sum')
                     0
#endif
#endif

#if STEP == 2
replicateM_ :: (Monad m) => Int -> m a -> m ()
#ifdef SOLUTION
replicateM_ 0 _      = return ()
replicateM_ n action = action >>= \_ -> replicateM_ (n-1) action

echoN :: Int -> IO ()
echoN n = replicateM_ n (getLine >>= putStrLn)
#endif

for :: (Monad m) => Int -> Int -> (Int -> m a) -> m ()
prop_for m n = execState (for m n (\i -> modify (+i))) 0 == sum [m..n]
#ifdef SOLUTION
for from to action = if from <= to
                     then do action from
                             for (from+1) to action
                     else return ()

countdown :: Int -> IO ()
countdown n = for (-n) 0 (\i -> print (-i))
#endif

while :: (Monad m) => m Bool -> m a -> m ()
prop_while n = execState (while (gets (<100)) (modify (+1))) n == max 100 n
#ifdef SOLUTION
while cond action = do b <- cond
                       if b then do action
                                    while cond action
                            else return ()

mantra :: IO ()
mantra = while (getLine >>= return . ("" ==))
               (putStrLn "The customer is always right.")
#endif
#endif

return []
main = $quickCheckAll >>= print
