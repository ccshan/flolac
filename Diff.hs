{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
#if STEP == 1
import Test.QuickCheck (quickCheckAll, (==>))
#else
import Test.QuickCheck (quickCheckAll, (==>), Arbitrary(arbitrary), frequency)
import System.Random (getStdRandom, randomR)
import Numeric (showGFloat)
#endif
import Control.Monad.Identity
import Control.Monad.Trans.State
import qualified Data.Map as M

data Expr = Lit Double
          | Add Expr Expr
          | Mul Expr Expr
          | Pow Expr Double
          | Exp Expr
          | Var Name
          | Let Name Expr Expr
  deriving (Eq, Show)
type Name = String

#if STEP >= 2
instance Arbitrary Expr where
  arbitrary = frequency [ (6, liftM  Lit arbitrary)
                        , (1, liftM2 Add arbitrary arbitrary)
                        , (1, liftM2 Mul arbitrary arbitrary)
                        , (1, liftM2 Pow arbitrary arbitrary)
                        , (1, liftM  Exp arbitrary) ]
                        -- not generating |Var|, |Let|
#endif

freeVars :: Expr -> M.Map Name ()
freeVars (Lit _)       = M.empty
freeVars (Var n)       = M.singleton n ()
freeVars (Add e1 e2)   = M.union (freeVars e1) (freeVars e2)
freeVars (Mul e1 e2)   = M.union (freeVars e1) (freeVars e2)
freeVars (Pow e _)     = freeVars e
freeVars (Exp e)       = freeVars e
freeVars (Let n rhs e) = M.union (freeVars rhs) (M.delete n (freeVars e))

sigmoid :: Expr -> Expr
sigmoid e = Add (Mul (Lit 2) (Pow (Add (Lit 1) (Exp (Mul (Lit (-1)) e)))
                                  (-1)))
                (Lit (-1))

eval :: (Monad m) => Expr -> M.Map Name Double -> m Double
prop_eval_Let                = runIdentity (eval (Let "x" (Add (Lit 3) (Lit (-1)))
                                                      (Mul (Var "x") (Var "x")))
                                                 M.empty)
                               == 4
prop_eval_square v           = abs (runIdentity (eval (Pow (Lit v) 2) M.empty) -
                                    runIdentity (eval (Mul (Lit v) (Lit v)) M.empty))
                               < 0.001
prop_eval_sigmoid0       env = 0 ==     runIdentity (eval (sigmoid (Lit 0)) env)
prop_eval_sigmoid1 v     env = let sv = runIdentity (eval (sigmoid (Lit v)) env)
                               in -1 <= sv && sv <= 1
prop_eval_sigmoid2 v1 v2 env = v1 < v2 ==>
                               let sv1 = runIdentity (eval (sigmoid (Lit v1)) env)
                                   sv2 = runIdentity (eval (sigmoid (Lit v2)) env)
                               in sv1 < sv2 || sv1 == sv2 && (v2 - v1 < 0.1 || v2 < -30 || v1 > 30)

eval (Lit v)       _   = return v
eval (Add e1 e2)   env = do v1 <- eval e1 env
                            v2 <- eval e2 env
                            return (v1 + v2)
eval (Mul e1 e2)   env = do v1 <- eval e1 env
                            v2 <- eval e2 env
                            return (v1 * v2)
eval (Var n)       env = do return (env M.! n)
eval (Let n rhs e) env = do v <- eval rhs env
                            eval e (M.insert n v env)
#ifdef SOLUTION
eval (Pow e y)     env = do v <- eval e env
                            return (v**y)
eval (Exp e)       env = do v <- eval e env
                            return (exp v)
#endif

sum_ :: [Expr] -> Expr
sum_ = foldl Add (Lit 0)

perceptron :: Expr -> (Expr, Expr) -> (Expr, Expr) -> Expr
perceptron a0 (a1,x) (a2,y) = sigmoid (sum_ [a0, Mul a1 x, Mul a2 y])

network :: Expr
network = Let "a" (perceptron (Var "a0") (Var "a1", Var "x") (Var "a2", Var "y"))
              (Let "b" (perceptron (Var "b0") (Var "b1", Var "x") (Var "b2", Var "y"))
                   (Let "c" (perceptron (Var "c0") (Var "c1", Var "a") (Var "c2", Var "b"))
                        (Var "c")))

prop_network = freeVars network
               == M.fromList (zip ("x": "y": sequence ["abc","012"])
                                  (repeat ()))

loss :: Expr
loss = sum_ [ Pow (Add (Let "x" (Lit x) (Let "y" (Lit y) network))
                       (Lit (negate expect)))
                  2
            | (x,y,expect) <- [ (-0.9, -0.9, -0.9)
                              , (-0.9,  0.9,  0.9)
                              , ( 0.9, -0.9,  0.9)
                              , ( 0.9,  0.9, -0.9) ] ]

prop_loss = freeVars loss
            == M.fromList (zip (sequence ["abc","012"])
                               (repeat ()))

#if STEP >= 2 && STEP <= 3
eval2 :: (Monad m) => Expr -> M.Map Name (Double, Double) -> m (Double, Double)
prop_eval2 e         = let v0    = runIdentity (eval  e M.empty)
                           (v,_) = runIdentity (eval2 e M.empty)
                       in v0 == v || isNaN v0 && isNaN v
prop_eval2_sigmoid u = runIdentity (eval2 (sigmoid (Var "x"))
                                          (M.singleton "x" (0,u)))
                       == (0, u/2)

eval2 (Lit v)       _   = return (v, 0)
eval2 (Add e1 e2)   env = do (v1, u1) <- eval2 e1 env
                             (v2, u2) <- eval2 e2 env
                             return (v1 + v2, u1 + u2)
eval2 (Mul e1 e2)   env = do (v1, u1) <- eval2 e1 env
                             (v2, u2) <- eval2 e2 env
                             return (v1 * v2, v1 * u2 + v2 * u1)
eval2 (Var n)       env = do return (env M.! n)
eval2 (Let n rhs e) env = do v <- eval2 rhs env
                             eval2 e (M.insert n v env)
#ifdef SOLUTION
eval2 (Pow e y)     env = do (v, u) <- eval2 e env
                             return (v**y, y * v**(y-1) * u)
eval2 (Exp e)       env = do (v, u) <- eval2 e env
                             return (exp v, exp v * u)
#endif
#endif

#if STEP == 4
type Delta = M.Map Name Double

eval3 :: (Monad m) => Expr -> M.Map Name (Double, Delta) -> m (Double, Delta)
prop_eval3 e         = let v0    = runIdentity (eval  e M.empty)
                           (v,_) = runIdentity (eval3 e M.empty)
                       in v0 == v || isNaN v0 && isNaN v
prop_eval3_sigmoid u = runIdentity (eval3 (sigmoid (Var "x"))
                                          (M.singleton "x" (0,u)))
                       == (0, M.map (/2) u)

eval3 (Lit v)       _   = return (v, M.empty)
eval3 (Add e1 e2)   env = do (v1, u1) <- eval3 e1 env
                             (v2, u2) <- eval3 e2 env
                             return (v1 + v2, dAdd u1 u2)
eval3 (Mul e1 e2)   env = do (v1, u1) <- eval3 e1 env
                             (v2, u2) <- eval3 e2 env
                             return (v1 * v2, dAdd (dScale v1 u2) (dScale v2 u1))
eval3 (Var n)       env = do return (env M.! n)
eval3 (Let n rhs e) env = do v <- eval3 rhs env
                             eval3 e (M.insert n v env)
#ifdef SOLUTION
eval3 (Pow e y)     env = do (v, u) <- eval3 e env
                             return (v**y, dScale (y * v**(y-1)) u)
eval3 (Exp e)       env = do (v, u) <- eval3 e env
                             return (exp v, dScale (exp v) u)
#endif

dAdd :: Delta -> Delta -> Delta
dAdd = M.unionWith (+)

dScale :: Double -> Delta -> Delta
dScale v = M.map (v *)
#endif

#if STEP == 5
data Delta = Zero
           | DAdd Delta Delta
           | DScale Double Delta
           | DVar DeltaId
           | DLet DeltaId Delta Delta
  deriving (Eq, Show)
data DeltaId = Name Name | Int Int
  deriving (Eq, Ord, Show)

data DeltaState = DeltaState Int DeltaBinds
  deriving (Eq, Show)
type DeltaBinds = [(DeltaId, Delta)] -- can be a mutable array that grows in |deltaLet|
type M = State DeltaState

eval4 :: Expr -> M.Map Name (Double, Delta) -> M (Double, Delta)
prop_eval4     = runState (eval4 (Mul (Lit 2) (Var "x"))
                                  (M.singleton "x" (3, DVar (Name "dx"))))
                          (DeltaState 0 [])
                 == ((6, DVar (Int 0)),
                     DeltaState 1 [ (Int 0, DAdd (DScale 2 (DVar (Name "dx")))
                                                 (DScale 3 Zero)) ])
prop_eval4_Let = runState (eval4 (Let "y" (Mul (Lit 2) (Var "x"))
                                      (Add (Var "y") (Var "y")))
                                 (M.singleton "x" (3, DVar (Name "dx"))))
                          (DeltaState 0 [])
                 == ((12, DVar (Int 1)),
                     DeltaState 2 [ (Int 1, DAdd (DVar (Int 0))
                                                 (DVar (Int 0)))
                                  , (Int 0, DAdd (DScale 2 (DVar (Name "dx")))
                                                 (DScale 3 Zero)) ])

eval4 (Lit v)       _   = return (v, Zero)
eval4 (Add e1 e2)   env = do (v1, u1) <- eval4 e1 env
                             (v2, u2) <- eval4 e2 env
                             u <- deltaLet (DAdd u1 u2)
                             return (v1 + v2, u)
eval4 (Mul e1 e2)   env = do (v1, u1) <- eval4 e1 env
                             (v2, u2) <- eval4 e2 env
                             u <- deltaLet (DAdd (DScale v1 u2) (DScale v2 u1))
                             return (v1 * v2, u)
eval4 (Var n)       env = do return (env M.! n)
eval4 (Let n rhs e) env = do v <- eval4 rhs env
                             eval4 e (M.insert n v env)
#ifdef SOLUTION
eval4 (Pow e y)     env = do (v, u) <- eval4 e env
                             u <- deltaLet (DScale (y * v**(y-1)) u)
                             return (v**y, u)
eval4 (Exp e)       env = do (v, u) <- eval4 e env
                             u <- deltaLet (DScale (exp v) u)
                             return (exp v, u)
#endif

deltaLet :: Delta -> M Delta
deltaLet delta = state (\(DeltaState next deltas) ->
                          (DVar (Int next), DeltaState (next+1) ((Int next, delta) : deltas)))

runDelta :: M (Double, Delta) -> (Double, Delta)
prop_runDelta = runDelta (eval4 (Let "y" (Mul (Lit 2) (Var "x"))
                                     (Add (Var "y") (Var "y")))
                                (M.singleton "x" (3, DVar (Name "dx"))))
                == (12, DLet (Int 0) (DAdd (DScale 2 (DVar (Name "dx")))
                                           (DScale 3 Zero))
                             (DLet (Int 1) (DAdd (DVar (Int 0))
                                                 (DVar (Int 0)))
                                   (DVar (Int 1))))
runDelta m = (res, foldl wrap delta binds)
  where ((res, delta), DeltaState _ binds) = runState m (DeltaState 0 [])
        wrap body (id, rhs)                = DLet id rhs body

type DeltaMap = M.Map DeltaId Double -- can be a mutable array that shrinks in |evalDelta|

evalDelta :: Double -> Delta -> DeltaMap -> DeltaMap
prop_evalDelta  = evalDelta 100
                            (DLet (Int 0) (DAdd (DScale 2 (DVar (Name "dx")))
                                                (DScale 3 Zero))
                                  (DLet (Int 1) (DAdd (DVar (Int 0))
                                                      (DVar (Int 0)))
                                        (DVar (Int 1))))
                            (M.fromList [(Name "dx", 1), (Name "dz", 42)])
                  == M.fromList [(Name "dx", 401), (Name "dz", 42)]
prop_evalDelta2 = evalDelta 100
                            (DLet (Int 1) (DAdd (DVar (Int 0))
                                                (DVar (Int 0)))
                                  (DVar (Int 1)))
                            (M.fromList [(Name "dx", 1), (Name "dz", 42)])
                  == M.fromList [(Name "dx", 1), (Name "dz", 42), (Int 0, 200)]
prop_evalDelta1 = evalDelta 200
                            (DAdd (DScale 2 (DVar (Name "dx")))
                                  (DScale 3 Zero))
                            (M.fromList [(Name "dx", 1), (Name "dz", 42)])
                  == M.fromList [(Name "dx", 401), (Name "dz", 42)]
evalDelta _ Zero             um = um
evalDelta x (DAdd u1 u2)     um = evalDelta x u2 (evalDelta x u1 um)
evalDelta x (DScale y u)     um = evalDelta (x * y) u um
evalDelta x (DVar uid)       um = M.insertWith (+) uid x um
evalDelta x (DLet uid u1 u2) um = let um2 = evalDelta x u2 um in
                                  case M.lookup uid um2 of
                                    Nothing -> um2
                                    Just x  -> evalDelta x u1 (M.delete uid um2)
#endif

#if STEP == 2
type Params = M.Map Name Double

randomParams :: IO Params
randomParams = traverse (\() -> getStdRandom (randomR (-2,2)))
                        (freeVars loss)
#endif
#if STEP >= 3
type Params = M.Map Name Inertia
data Inertia = Inertia Double Double
  deriving (Eq, Show)

randomParams :: IO Params
#ifdef SOLUTION
randomParams = traverse (\() -> do r <- getStdRandom (randomR (-2,2))
                                   return (Inertia r 0))
                        (freeVars loss)
#else
randomParams = traverse _
                        (freeVars loss)
#endif
#endif

#if STEP == 2
stepParams :: Params -> Params
stepParams params =
  let stepParam :: Name -> Double -> Double
      stepParam n v =
        let dualize nn vv = (vv, if n == nn then 1 else 0)
            newMomentum = - 0.1 * snd (runIdentity (eval2 loss (M.mapWithKey dualize params)))
        in v + newMomentum
  in M.mapWithKey stepParam params

optimize :: Params -> IO ()
optimize params = do
  mapM_ (\v -> putStr (showF v ++ " ")) params
  putStrLn ("=> " ++ showF (runIdentity (eval loss params)))
  optimize (iterate stepParams params !! 1000)
#endif

#if STEP == 3
stepParams :: Params -> Params
stepParams params =
  let stepParam :: Name -> Inertia -> Inertia
      stepParam n (Inertia v oldMomentum) =
#ifdef SOLUTION
        let dualize nn (Inertia vv _) = (vv, if n == nn then 1 else 0)
            newMomentum = 0.9 * oldMomentum
                        - 0.1 * snd (runIdentity (eval2 loss (M.mapWithKey dualize params)))
        in Inertia (v + newMomentum) newMomentum
#else
        _
#endif
  in M.mapWithKey stepParam params
#endif

#if STEP == 4
stepParams :: Params -> Params
stepParams params =
  let dualize n (Inertia v _) = (v, M.singleton n 1)
      grad = snd (runIdentity (eval3 loss (M.mapWithKey dualize params)))
      stepParam :: Double -> Inertia -> Inertia
      stepParam u (Inertia v oldMomentum) =
        let newMomentum = 0.9 * oldMomentum - 0.1 * u
        in Inertia (v + newMomentum) newMomentum
  in M.union (M.intersectionWith stepParam grad params)
             (M.map (stepParam 0) (M.difference params grad))
#endif

#if STEP == 5
stepParams :: Params -> Params
stepParams params =
  let dualize n (Inertia v _) = (v, DVar (Name n))
      (_, u) = runDelta (eval4 loss (M.mapWithKey dualize params))
      grad = M.mapKeysMonotonic (\(Name n) -> n) (evalDelta 1 u M.empty)
      stepParam :: Double -> Inertia -> Inertia
      stepParam u (Inertia v oldMomentum) =
        let newMomentum = 0.9 * oldMomentum - 0.1 * u
        in Inertia (v + newMomentum) newMomentum
  in M.union (M.intersectionWith stepParam grad params)
             (M.map (stepParam 0) (M.difference params grad))
#endif

#if STEP >= 3
optimize :: Params -> IO ()
optimize params = do
#ifdef SOLUTION
  params0 <- traverse (\(Inertia v _) -> putStr (showF v ++ " ") >> return v) params
  putStrLn ("=> " ++ showF (runIdentity (eval loss params0)))
#else
  _
#endif
  optimize (iterate stepParams params !! 1000)
#endif

#if STEP >= 2
showF :: Double -> String
showF v = showGFloat (Just 3) v ""
#endif

return []
main = $quickCheckAll >>= print
