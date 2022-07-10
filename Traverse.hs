%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
\end{code}
\end{comment}

\begin{code}
import Control.Monad.Trans.State
\end{code}

\begin{code}
renumber :: [a] -> [Int]
prop_renumber = renumber "hello" == [0,1,2,3,4]
renumber xs = evalState (traverse (\_ -> state (\s -> (s, s+1))) xs) 0

choices :: [Int] -> [[Int]]
prop_choices = choices [2,3] == [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]
choices = traverse (\n -> [0..n-1])

dec :: [Int] -> Maybe [Int]
prop_dec1  = dec [2,5,3] == Just [1,4,2]
prop_dec2  = dec [2,0,3] == Nothing
dec = traverse (\n -> if n>0 then Just (n-1) else Nothing)
\end{code}

\begin{solution}
\begin{code}
prefixSum :: [Int] -> [Int]
prop_prefixSum = prefixSum [10,30,0,2,-1] == [10,40,40,42,41]
prefixSum ns = evalState (traverse (\n -> state (\s -> (s+n, s+n))) ns) 0

cartesian :: [[a]] -> [[a]]
prop_cartesian = cartesian ["hi","bye"] == ["hb","hy","he","ib","iy","ie"]
cartesian = traverse id

recips :: [Double] -> Maybe [Double]
prop_recips1  = recips [1,5,2] == Just [1,0.2,0.5]
prop_recips2  = recips [1,0,2] == Nothing
recips = traverse (\x -> if x == 0 then Nothing else Just (1/x))
\end{code}
\end{solution}

\begin{code}
data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

traverseTree :: (Monad m) => (Int -> m Int) -> Tree -> m Tree
traverseTree f (Leaf n)        = do  n' <- f n
                                     return (Leaf n')
traverseTree f (Branch t1 t2)  = do  t1' <- traverseTree f t1
                                     t2' <- traverseTree f t2
                                     return (Branch t1' t2')
\end{code}

\begin{solution}
\begin{code}
edit :: (Show a, Read a) => a -> IO a
edit a = do  print a
             str <- getLine
             return (fromMaybe a (readMaybe str))
\end{code}
\end{solution}

\begin{code}
forever action = action >>= \_ -> forever action

replicateM_ :: (Monad m) => Int -> m a -> m ()

for :: (Monad m) => Int -> Int -> (Int -> m a) -> m ()
prop_for m n = execState (for m n (\i -> modify (+i))) 0 == sum [m..n]

while :: (Monad m) => m Bool -> m a -> m ()
prop_while n = execState (while (gets (<100)) (modify (+1))) n == max 100 n
\end{code}

\begin{solution}
\begin{code}
forever :: (Monad m) => m a -> m b

echo :: IO b
echo = forever (getLine >>= putStrLn)

replicateM_ 0 _       = return ()
replicateM_ n action  = action >>= \_ -> replicateM_ (n-1) action

echoN :: Int -> IO ()
echoN n = replicateM_ n (getLine >>= putStrLn)

for from to action  =  if from <= to
                       then do  action from
                                for (from+1) to action
                       else return ()

countdown :: Int -> IO ()
countdown n = for (-n) 0 (\i -> print (-i))

while cond action = do  b <- cond
                        if b  then do  action
                                       while cond action
                              else return ()
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
