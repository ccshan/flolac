{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Control.Monad.Trans.State

forever action = action >> forever action

replicateM_ :: (Monad m) => Int -> m a -> m ()

for :: (Monad m) => Int -> Int -> (Int -> m a) -> m ()
prop_for m n = execState (for m n (\i -> modify (+i))) 0 == sum [m..n]

while :: (Monad m) => m Bool -> m a -> m ()
prop_while n = execState (while (gets (<100)) (modify (+1))) n == max 100 n

#ifdef SOLUTION
forever :: (Monad m) => m a -> m b

echo :: IO b
echo = forever (getLine >>= putStrLn)

replicateM_ 0 _      = return ()
replicateM_ n action = action >>= \_ -> replicateM_ (n-1) action

echoN :: Int -> IO ()
echoN n = replicateM_ n (getLine >>= putStrLn)

for from to action = if from <= to
                     then do action from
                             for (from+1) to action
                     else return ()

countdown :: Int -> IO ()
countdown n = for (-n) 0 (\i -> print (-i))

while cond action = do b <- cond
                       if b then do action
                                    while cond action
                            else return ()
#endif

return []
main = $quickCheckAll >>= print
