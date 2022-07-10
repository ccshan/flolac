{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

sumTree      :: Tree -> Int
productTree  :: Tree -> Int
incTree      :: Tree -> Tree

prop_sum     = sumTree     (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
               ==          10
prop_product = productTree (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
               ==          30
prop_inc     = incTree     (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
               ==          (Branch (Leaf 4) (Branch (Leaf 6) (Leaf 3)))

#ifdef SOLUTION
sumTree (Leaf n)           = n
sumTree (Branch t1 t2)     = sumTree t1 + sumTree t2

productTree (Leaf n)       = n
productTree (Branch t1 t2) = productTree t1 * productTree t2

incTree (Leaf n)           = Leaf (n+1)
incTree (Branch t1 t2)     = Branch (incTree t1) (incTree t2)
#endif

return []
main = $quickCheckAll >>= print
