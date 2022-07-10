{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)

#if STEP == 1
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Get | Put Expr
  deriving (Eq, Show)
type State = Int
#endif
#if STEP == 2
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
          | New Expr | Get Expr | Put Expr Expr
  deriving (Eq, Show)
type State = [Int]
#endif

eval :: Expr -> State -> (Int, State)
prop_eval1      s = eval (Add (Lit 2) (Lit 3))               s  == (5, s)
prop_eval2      s = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3)) s  == (6, s)
prop_evalLit v  s = eval (Lit v)                             s  == (v, s)
#if STEP == 1
prop_evalState1   = eval Get                                 50 == (50, 50)
prop_evalState2   = eval (Add Get (Lit 1))                   50 == (51, 50)
prop_evalState3   = eval (Put (Lit 51))                      50 == (51, 51)
prop_evalState4   = eval (Put (Add Get (Lit 1)))             50 == (51, 51)
prop_evalState5   = eval (Add Get (Put (Add Get (Lit 1))))   50 == (101, 51)
prop_evalState6   = eval (Add (Put (Add Get (Lit 1))) Get)   50 == (102, 51)
prop_evalPutGet s = eval (Put Get)                           s  == eval Get s
#endif
#if STEP == 2
prop_evalNew      = eval (New (New (Lit 100)))               [] == (1, [100,0])
prop_evalGet      = eval (Add (Lit 1) (Get (New (Lit 100)))) [] == (101, [100])
prop_evalPut      = eval (Put (New (Lit 10)) (New (Lit 20))) [] == (1, [1,20])
#endif

#ifdef SOLUTION
eval (Lit v)     s = (v, s)
eval (Add e1 e2) s = let (v1, s1) = eval e1 s
                         (v2, s2) = eval e2 s1
                     in (v1 + v2, s2)
eval (Mul e1 e2) s = let (v1, s1) = eval e1 s
                         (v2, s2) = eval e2 s1
                     in (v1 * v2, s2)
#if STEP == 1
eval Get         s = (s, s)
eval (Put e)     s = let (v, _) = eval e s
                     in (v, v)
#endif
#if STEP ==2
eval (New e)     s = let (v, s') = eval e s
                     in (length s', s' ++ [v])
eval (Get e)     s = let (v, s') = eval e s
                     in (s' !! v, s')
eval (Put e1 e2) s = let (v1, s1) = eval e1 s
                         (v2, s2) = eval e2 s1
                         (s2before, _:s2after) = splitAt v1 s2
                     in (v2, s2before ++ v2 : s2after)
#endif
#endif

return []
main = $quickCheckAll >>= print
