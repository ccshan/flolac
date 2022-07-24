{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Control.Monad.Trans.State
#ifdef SOLUTION
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
#endif

renumber :: [a] -> [Int]
prop_renumber = renumber "hello" == [0,1,2,3,4]
renumber xs = evalState (traverse (\_ -> state (\s -> (s, s+1))) xs) 0

choices :: [Int] -> [[Int]]
prop_choices = choices [2,3] == [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]
choices = traverse (\n -> [0..n-1])

dec :: [Int] -> Maybe [Int]
prop_dec1 = dec [2,5,3] == Just [1,4,2]
prop_dec2 = dec [2,0,3] == Nothing
dec = traverse (\n -> if n>0 then Just (n-1) else Nothing)

#ifdef SOLUTION
prefixSum :: [Int] -> [Int]
prop_prefixSum = prefixSum [10,30,0,2,-1] == [10,40,40,42,41]
prefixSum ns = evalState (traverse (\n -> state (\s -> (s+n, s+n))) ns) 0

cartesian :: [[a]] -> [[a]]
prop_cartesian = cartesian ["hi","bye"] == ["hb","hy","he","ib","iy","ie"]
cartesian = traverse id

recips :: [Double] -> Maybe [Double]
prop_recips1 = recips [1,5,2] == Just [1,0.2,0.5]
prop_recips2 = recips [1,0,2] == Nothing
recips = traverse (\x -> if x == 0 then Nothing else Just (1/x))
#endif

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

traverseTree :: (Monad m) => (Int -> m Int) -> Tree -> m Tree
traverseTree f (Leaf n)       = do n' <- f n
                                   return (Leaf n')
traverseTree f (Branch t1 t2) = do t1' <- traverseTree f t1
                                   t2' <- traverseTree f t2
                                   return (Branch t1' t2')

#ifdef SOLUTION
edit :: (Show a, Read a) => a -> IO a
edit a = do print a
            str <- getLine
            return (fromMaybe a (readMaybe str))
#endif

return []
main = $quickCheckAll >>= print
