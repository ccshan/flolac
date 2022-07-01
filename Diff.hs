{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -W #-}

import Test.QuickCheck
import System.Random
import Control.Monad.Trans.State
import Numeric (showGFloat)
import qualified Data.Map as M
import qualified Data.Set as S

data E = Lit Double | Var N | Add E E | Mul E E | Pow E Double | Exp E | Let N E E
    deriving (Eq, Show)
type N = String
type Env = M.Map N

fv :: E -> S.Set N
fv (Lit _)       = S.empty
fv (Var n)       = S.singleton n
fv (Add e1 e2)   = S.union (fv e1) (fv e2)
fv (Mul e1 e2)   = S.union (fv e1) (fv e2)
fv (Pow e _)     = fv e
fv (Exp e)       = fv e
fv (Let n rhs e) = S.union (fv rhs) (S.delete n (fv e))

eval :: E -> Env Double -> Double
eval (Lit x)       _   = x
eval (Var n)       env = env M.! n
eval (Add e1 e2)   env = eval e1 env + eval e2 env
eval (Mul e1 e2)   env = eval e1 env * eval e2 env
eval (Pow e y)     env = eval e env ** y
eval (Exp e)       env = exp (eval e env)
eval (Let n rhs e) env = eval e (M.insert n (eval rhs env) env)

prop_eval1 = eval (Let "x" (Add (Lit 3) (Lit (-1)))
                       (Mul (Var "x") (Add (Var "x") (Exp (Lit 0)))))
                  M.empty
             == 6
prop_eval2 x = let x' = Lit (fromInteger x)
               in eval (Pow x' 2) M.empty == eval (Mul x' x') M.empty

data Dual = Dual Double Diff
    deriving (Eq, Show)
data Diff = Zero | DVar V | DAdd Diff Diff | DMul Double Diff
    deriving (Eq, Show)
newtype V = V Int
    deriving (Eq, Ord, Show, Enum)

data S = S V [Diff]
    deriving (Eq, Show)
type M = State S

let_ :: Diff -> M Diff
let_ d = state (\(S next ds) -> (DVar next, S (succ next) (d : ds)))

diff :: E -> Env Dual -> M Dual
diff (Lit x)       _   = return (Dual x Zero)
diff (Var v)       env = return (env M.! v)
diff (Add e1 e2)   env = do Dual x1 d1 <- diff e1 env
                            Dual x2 d2 <- diff e2 env
                            d <- let_ (DAdd d1 d2)
                            return (Dual (x1 + x2) d)
diff (Mul e1 e2)   env = do Dual x1 d1 <- diff e1 env
                            Dual x2 d2 <- diff e2 env
                            d <- let_ (DAdd (DMul x2 d1) (DMul x1 d2))
                            return (Dual (x1 * x2) d)
diff (Pow e y)     env = do Dual x dx <- diff e env
                            d <- let_ (DMul (y * x ** (y - 1)) dx)
                            return (Dual (x ** y) d)
diff (Exp e)       env = do Dual x dx <- diff e env
                            d <- let_ (DMul (exp x) dx)
                            return (Dual (exp x) d)
diff (Let n rhs e) env = do dual <- diff rhs env
                            diff e (M.insert n dual env)

type Accum = M.Map V Double

accum :: Double -> Diff -> Accum -> Accum
accum _      Zero         = id
accum factor (DVar v)     = M.insertWith (+) v factor
accum factor (DAdd d1 d2) = accum factor d1 . accum factor d2
accum factor (DMul x d)   = accum (factor * x) d

runDiff :: E -> Env Dual -> (Double, M.Map V Double)
runDiff e env =
  let (Dual y dy, S next ds) = runState (diff e env) (S (V 0) [])
      unravel (v, acc) d =
        (pred v, case M.findWithDefault 0 v acc of
                   0 -> acc
                   factor -> accum factor d (M.delete v acc))
  in (y, snd (foldl unravel (next, M.singleton next 1) (dy:ds)))

prop_runDiff1 = runDiff (Mul (Var "x") (Var "x"))
                        (M.singleton "x" (Dual 3 (DVar (V (-1)))))
                == (9, M.singleton (V (-1)) 6)
prop_runDiff2 = runDiff (Exp (Add (Var "x") (Mul (Lit 2) (Var "y"))))
                        (M.fromList [("x", Dual 3 (DVar (V (-1)))),
                                     ("y", Dual 4 (DVar (V (-2))))])
                == (exp 11, M.fromList [(V (-1), exp 11),
                                        (V (-2), exp 11 * 2)])
prop_runDiff3 = runDiff (Pow (Add (Var "x") (Mul (Lit 2) (Var "y"))) 3)
                        (M.fromList [("x", Dual 3 (DVar (V (-1)))),
                                     ("y", Dual 4 (DVar (V (-2))))])
                == (11**3, M.fromList [(V (-1), 3*11**2),
                                       (V (-2), 3*11**2*2)])

runDiff' :: E -> Env Double -> (Double, Env Double)
runDiff' e env = (y, M.map (\(_, v) -> md M.! v) mv)
  where mv = evalState (mapM (\x -> state (\v -> ((x, v), pred v))) env)
                       (V (-1))
        mx = M.map (\(x, v) -> Dual x (DVar v)) mv
        (y, md) = runDiff e mx

prop_runDiff'1 = runDiff' (Mul (Var "x") (Var "x"))
                          (M.singleton "x" 3)
                 == (9, M.singleton "x" 6)
prop_runDiff'2 = runDiff' (Exp (Add (Var "x") (Mul (Lit 2) (Var "y"))))
                          (M.fromList [("x", 3), ("y", 4)])
                 == (exp 11, M.fromList [("x", exp 11), ("y", exp 11 * 2)])
prop_runDiff'3 = runDiff' (Pow (Add (Var "x") (Mul (Lit 2) (Var "y"))) 3)
                          (M.fromList [("x", 3), ("y", 4)])
                 == (11**3, M.fromList [("x", 3*11**2), ("y", 3*11**2*2)])

sigmoid :: E -> E
sigmoid = Add (Lit (-1))
        . Mul (Lit 2)
        . flip Pow (-1)
        . Add (Lit 1)
        . Exp
        . Mul (Lit (-1))

let_perceptron :: N -> [E] -> E -> E
let_perceptron n rhs =
  Let n (sigmoid (foldl activate (Var (n ++ "0")) (zip [1..] rhs)))
  where activate bias (i, input) = Add bias (Mul (Var (n ++ show i)) input)

loss :: E -> Double -> E
loss actual expect = Pow (Add actual (Lit (negate expect))) 2

network :: E
network = let_perceptron "a" [Var "x", Var "y"]
        $ let_perceptron "b" [Var "x", Var "y"]
        $ let_perceptron "c" [Var "a", Var "b"]
        $ Var "c"

params :: S.Set N
params = S.fromList [ n ++ show i | n <- ["a", "b", "c"], i <- [0..2] ]

prop_fv_network = fv network == S.union params (S.fromList ["x", "y"])

networkTotalLoss :: E
networkTotalLoss = foldl1 Add [ loss (Let "x" (Lit x) (Let "y" (Lit y) network))
                                     expect
                              | (x,y,expect) <- [(-0.9, -0.9, -0.9),
                                                 (-0.9,  0.9,  0.9),
                                                 ( 0.9, -0.9,  0.9),
                                                 ( 0.9,  0.9, -0.9)] ]

prop_fv_networkTotalLoss = fv networkTotalLoss == params

data Inertia = Inertia Double Double
    deriving (Eq, Show)

randomEnv :: S.Set N -> IO (Env Inertia)
randomEnv vars = mapM pick (M.fromSet (const ()) vars)
  where pick () = flip Inertia 0 <$> getStdRandom (randomR (-2,2))

step :: (Env Double  -> (Double, Env Double ))
     -> (Env Inertia -> (Double, Env Inertia))
step g env = (y, M.mapWithKey update env)
  where (y, grad) = g (M.map (\(Inertia x _) -> x) env)
        update n (Inertia x vel) = Inertia (x + vel') vel'
          where vel' = 0.8 * vel - 0.1 * grad M.! n

main :: IO ()
main = randomEnv params >>= loop
  where loop env = do mapM_ (\(Inertia x _) -> putStr (showGFloat (Just 3) x " ")) env
                      let (loss, env') = step (runDiff' networkTotalLoss) env
                      putStrLn (showGFloat (Just 3) loss "")
                      loop env'

return []
runTests = $quickCheckAll
