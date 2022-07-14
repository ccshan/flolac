{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import qualified Data.Set as S

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

eqSet :: [Int] -> [Int] -> Bool
-- ^Check if the two given lists are equal as sets
eqSet s1 s2 = S.fromList s1 == S.fromList s2

blackjack :: Tree -> [Int]
-- ^Compute all possible sums of subsets of the given list
-- whose prefix sums never exceed 21
prop_blackjack1 = eqSet (blackjack (Branch (Branch (Leaf 10) (Leaf 9))
                                           (Branch (Leaf 12) (Leaf 11))))
                        [0,9,10,11,12,19,20,21]
prop_blackjack2 = eqSet (blackjack (Branch (Branch (Leaf 11) (Leaf (-1)))
                                           (Leaf 11)))
                        [-1,0,10,11,21]
prop_blackjack3 = eqSet (blackjack (Branch (Leaf 11)
                                           (Branch (Leaf 11) (Leaf (-1)))))
                        [-1,0,10,11]
blackjack t = blackjack' t 0

blackjack' :: Tree -> Int -> [Int]
-- ^Compute all possible sums of subsets of the given list
-- whose prefix sums never exceed 21
-- /Accumulator/: `total` is the sum of the numbers chosen so far

#ifdef SOLUTION
blackjack' (Leaf n)       total = if total + n > 21 then [total]
                                  else [total, total + n]
blackjack' (Branch t1 t2) total = concatMap (blackjack' t2) (blackjack' t1 total)
#endif

return []
main = $quickCheckAll >>= print
