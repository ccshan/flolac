{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
#if STEP == 2
import qualified Data.Set as S
#endif

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

#if STEP == 1
sumTree :: Tree -> Int
-- ^Add together all the numbers in the given tree
prop_sum = sumTree (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
           == 10

sumTree' :: Tree -> Int -> Int
-- ^Add together all the numbers in the given tree
-- /Accumulator/: `result` is the sum of the numbers seen so far

#ifdef SOLUTION
sumTree t = sumTree' t 0

sumTree' (Leaf n)       result = result + n
sumTree' (Branch t1 t2) result = let result1 = sumTree' t1 result
                                     result2 = sumTree' t2 result1
                                 in result2
#endif
#endif

#if STEP == 2
relabel :: Tree -> Tree
-- ^Change the numbers in the given tree to successive integers
prop_relabel = relabel (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
               ==      (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)))

relabel' :: Tree -> Int -> (Tree, Int)
-- ^Change the numbers in the given tree to successive integers
-- /State/: `next` and the `Int` returned are the last number assigned so far

#ifdef SOLUTION
relabel t = let (t', _) = relabel' t 0 in t'

relabel' (Leaf _)       next = (Leaf (next+1), next+1)
relabel' (Branch t1 t2) next = let (t1', next1) = relabel' t1 next
                                   (t2', next2) = relabel' t2 next1
                               in (Branch t1' t2', next2)
#endif

unique :: Tree -> Bool
-- ^Check if the numbers in the given tree are all different
prop_unique1 = unique (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2))) == True
prop_unique2 = unique (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 3))) == False

unique'  :: Tree -> S.Set Int -> (Bool, S.Set Int)
-- ^Check if the numbers in the given tree are all different
-- /State/: `seen` and the `S.Set Int` returned are the numbers seen so far
unique'' :: Tree ->              (Bool, S.Set Int)
-- ^Check if the numbers in the given tree are all different
-- and also return the set of numbers in the given tree

#ifdef SOLUTION
unique t = let (u, _) = unique' t S.empty in u

unique' (Leaf n)       seen = if S.member n seen then (False, seen)
                              else (True, S.insert n seen)
unique' (Branch t1 t2) seen = let (u1, seen1) = unique' t1 seen
                                  (u2, seen2) = unique' t2 seen1
                              in (u1 && u2, seen2)

-- unique t = let (u, _) = unique'' t in u

unique'' (Leaf n)       = (True, S.singleton n)
unique'' (Branch t1 t2) = let (u1, seen1) = unique'' t1
                              (u2, seen2) = unique'' t2
                          in (u1 && u2 && S.disjoint seen1 seen2, S.union seen1 seen2)
#endif
#endif

return []
main = $quickCheckAll >>= print
