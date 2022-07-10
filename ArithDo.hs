{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
#if STEP == 1
import Control.Monad.Trans.State (State, state, runState)
#endif

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
#if STEP == 1
          | Get | Put Expr
#endif
#if STEP == 2
          | Div Expr Expr
#endif
#if STEP == 3
          | Amb Expr Expr
#endif
#if STEP == 4
          | Input | Output Expr
#endif
  deriving (Eq, Show)

#if STEP == 1
eval :: Expr -> State Int Int
prop_eval1      s = runState (eval (Add (Lit 2) (Lit 3)))               s  == (5,s)
prop_eval2      s = runState (eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))) s  == (6,s)
prop_evalLit v  s = runState (eval (Lit v))                             s  == (v,s)
prop_evalState1   = runState (eval Get)                                 50 == (50, 50)
prop_evalState2   = runState (eval (Add Get (Lit 1)))                   50 == (51, 50)
prop_evalState3   = runState (eval (Put (Lit 51)))                      50 == (51, 51)
prop_evalState4   = runState (eval (Put (Add Get (Lit 1))))             50 == (51, 51)
prop_evalState5   = runState (eval (Add Get (Put (Add Get (Lit 1)))))   50 == (101, 51)
prop_evalState6   = runState (eval (Add (Put (Add Get (Lit 1))) Get))   50 == (102, 51)
prop_evalPutGet s = runState (eval (Put Get))                           s  == runState (eval Get) s
#endif
#if STEP == 2
eval :: Expr -> Maybe Int
prop_eval1     = eval (Add (Lit 2) (Lit 3))                                == Just 5
prop_eval2     = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))                  == Just 6
prop_evalLit v = eval (Lit v)                                              == Just v
prop_evalDiv1  = eval (Div (Lit 6) (Lit 2))                                == Just 3
prop_evalDiv2  = eval (Mul (Div (Lit 3) (Add (Lit 1) (Lit (-1)))) (Lit 2)) == Nothing
#endif
#if STEP == 3
eval :: Expr -> [Int]
prop_eval1     = eval (Add (Lit 2) (Lit 3))                             == [5]
prop_eval2     = eval (Mul (Add (Lit 1) (Lit 1)) (Lit 3))               == [6]
prop_evalLit v = eval (Lit v)                                           == [v]
prop_evalAmb   = eval (Mul (Add (Lit 1) (Amb (Lit 0) (Lit 1))) (Lit 3)) == [3,6]
#endif
#if STEP == 4
eval :: Expr -> IO Int
#endif

eval (Lit v)     = return v
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (Mul e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
#ifdef SOLUTION
#if STEP == 1
eval Get         = state (\s -> (s, s))
eval (Put e)     = do v <- eval e
                      state (\_ -> (v, v))
#endif
#if STEP == 2
eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0 then Nothing else return (v1 `div` v2)
#endif
#if STEP == 3
eval (Amb e1 e2) = eval e1 ++ eval e2
#endif
#if STEP == 4
eval Input       = do str <- getLine
                      return (read str)
eval (Output e)  = do v <- eval e
                      print v
                      return v
#endif
#endif

return []
main = $quickCheckAll >>= print
