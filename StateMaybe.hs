{-# OPTIONS -W #-}
#if STEP <= 2
import Control.Monad (liftM, ap)
#endif
#if STEP == 4
import Control.Monad.Trans.Maybe
#endif
#if STEP >= 3
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Applicative (empty)
#endif

#if defined(SOLUTION) && STEP == 4
#define LIFT4(action) lift action
#else
#define LIFT4(action) action
#endif

main :: IO ()
main = do putStr "puzzle1 = "
          print (runM puzzle1 100)
          putStr "puzzle2 = "
          print (runM puzzle2 100)

puzzle1 :: M Int
puzzle1 = eval (Put (Div (Put (Div Get (Lit 2))) (Lit 0)))

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
          | Get | Put Expr | Div Expr Expr

eval :: Expr -> M Int
eval (Lit v)     = return v
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (Mul e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval Get         = do LIFT4(get)
eval (Put e)     = do v <- eval e
                      LIFT4((put v))
                      return v
eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      divide v1 v2

puzzle2 :: M ()
puzzle2 = do s0 <- LIFT4(get)
             s1 <- divide s0 2
             LIFT4((put s1))
             s2 <- divide s1 0
             LIFT4((put s2))

#if STEP <= 2
newtype M a = M {runM ::
#ifdef SOLUTION
#if STEP == 1
                         Int -> Maybe (a, Int)}
#endif
#if STEP == 2
                         Int -> (Maybe a, Int)}
#endif
#endif

instance Functor M where fmap = liftM

instance Applicative M where pure = return; (<*>) = ap

#ifdef SOLUTION
#if STEP == 1
instance Monad M where
  return a = M (\s -> Just (a, s))
  m >>= k  = M (\s -> case runM m s of Nothing      -> Nothing
                                       Just (a, s') -> runM (k a) s')
#endif
#if STEP == 2
instance Monad M where
  return a = M (\s -> (Just a, s))
  m >>= k  = M (\s -> case runM m s of (Nothing, s') -> (Nothing, s')
                                       (Just a , s') -> runM (k a) s')
#endif
#endif

get    :: M Int
put    :: Int -> M ()
divide :: Int -> Int -> M Int

#ifdef SOLUTION
#if STEP == 1
get        = M (\s -> Just (s, s))
put s      = M (\_ -> Just ((), s))
divide _ 0 = M (\_ -> Nothing)
divide x y = return (x `div` y)
#endif
#if STEP == 2
get        = M (\s -> (Just s, s))
put s      = M (\_ -> (Just (), s))
divide _ 0 = M (\s -> (Nothing, s))
divide x y = return (x `div` y)
#endif
#endif
#endif

#if STEP == 3
type M = StateT Int Maybe

runM :: M a -> Int -> Maybe (a, Int)
runM = runStateT

divide :: Int -> Int -> M Int
divide _ 0 =
#ifdef SOLUTION
             empty -- or `lift Nothing`
#endif
divide x y =
#ifdef SOLUTION
             return (x `div` y)
#endif
#endif

#if STEP == 4
type M = MaybeT (State Int)

runM :: M a -> Int -> (Maybe a, Int)
runM = runState . runMaybeT

divide :: Int -> Int -> M Int
divide _ 0 =
#ifdef SOLUTION
             empty
#endif
divide x y =
#ifdef SOLUTION
             return (x `div` y)
#endif
#endif
