{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)

type M a = [a]

a = 9                    :: Int
k = (\n -> [1..n])       :: Int -> M Int
m = [5,3]                :: M Int
l = (\n -> [n, n * 10])  :: Int -> M Int

prop_leftIdentity   =  (return a >>= k) == k a
prop_rightIdentity  =  (m >>= return) == m
prop_associativity  =  (m >>= (\a -> k a >>= l)) == ((m >>= k) >>= l)

return []
main = $quickCheckAll >>= print
