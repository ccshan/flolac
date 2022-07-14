{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Amb Expr Expr
  deriving (Eq, Show)

eval :: Expr -> [Int]
-- ^Compute the possible result values of the given expression
prop_eval1     = eval (Add (Lit 2) (Lit 3))                             == [5]
prop_eval2     = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))               == [6]
prop_evalLit v = eval (Lit v)                                           == [v]
prop_evalAmb   = eval (Mul (Add (Lit 1) (Amb (Lit 0) (Lit 1))) (Lit 3)) == [3,6]

#ifdef SOLUTION
eval (Lit v)     = [v]
eval (Add e1 e2) = concatMap (\v1 -> map (\v2 -> v1+v2)
                                         (eval e2))
                             (eval e1)
eval (Mul e1 e2) = concatMap (\v1 -> map (\v2 -> v1*v2)
                                         (eval e2))
                             (eval e1)
eval (Amb e1 e2) = eval e1 ++ eval e2
#endif

return []
main = $quickCheckAll >>= print
