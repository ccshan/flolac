{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Control.Monad (join)

my_bind :: (Monad m) => m a -> (a -> m b) -> m b
prop_bind :: [Int] -> [Char] -> [Char] -> [Char] -> Bool
prop_bind m n0 n1 n2 = (my_bind m k) == (m >>= k)
  where k i = case mod i 3 of 0 -> n0
                              1 -> n1
                              _ -> n2

my_fmap :: (Monad m) => (a -> b) -> m a -> m b
prop_fmap :: [Int] -> Char -> Char -> Char -> Bool
prop_fmap m c0 c1 c2 = my_fmap f m == fmap f m
  where f i = case mod i 3 of 0 -> c0
                              1 -> c1
                              _ -> c2

my_join :: (Monad m) => m (m a) -> m a
prop_join :: [[Int]] -> Bool
prop_join m = my_join m == join m

#ifdef SOLUTION
my_bind m k = join (fmap k m)

my_fmap f m = m >>= \a -> return (f a)
my_join m   = m >>= id
#endif

return []
main = $quickCheckAll >>= print
