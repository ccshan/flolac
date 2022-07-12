{-# OPTIONS -W #-}
#if STEP <= 2
import Control.Monad (liftM, ap)
#endif
#ifdef SOLUTION
#if STEP == 2
import Control.Monad.Trans.State (runState, state)
#endif
#endif
#if STEP == 4
import Control.Monad.Trans.List
#endif
#if STEP >= 3
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Applicative (empty, (<|>))
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
#if STEP == 4
          if runM ((law_m >> law_k) >> law_l) 100 ==
             runM (law_m >> (law_k >> law_l)) 100
             then putStrLn "Keep trying to disprove monad associativity..."
             else putStrLn "Congratulations! You disproved monad associativity"
#endif

puzzle1 :: M Int
puzzle1 = eval (Amb (Put (Add Get (Lit 1)))
                    (Put (Add Get (Lit 2))))

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
          | Get | Put Expr | Amb Expr Expr

eval :: Expr -> M Int
eval (Lit v)     = return v
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (Mul e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval Get         = LIFT4(get)
eval (Put e)     = do v <- eval e
                      LIFT4((put v))
                      return v
eval (Amb e1 e2) = amb [eval e1, eval e2]

puzzle2 :: M ()
puzzle2 = amb [ do s <- LIFT4(get)
                   LIFT4((put (s+1)))
              , do s <- LIFT4(get)
                   LIFT4((put (s+2)))]

#if STEP <= 2
newtype M a = M {runM ::
#ifdef SOLUTION
#if STEP == 1
                         Int -> [(a, Int)]}
#endif
#if STEP == 2
                         Int -> ([a], Int)}
#endif
#endif

instance Functor M where fmap = liftM

instance Applicative M where pure = return; (<*>) = ap

#ifdef SOLUTION
#if STEP == 1
instance Monad M where
  return a = M (\s -> [(a, s)])
  m >>= k  = M (\s -> concatMap (\(a, s') -> runM (k a) s')
                                (runM m s))
#endif
#if STEP == 2
instance Monad M where
  return a = M (\s -> ([a], s))
  m >>= k  = M (runState (do as <- state (runM m)
                             bss <- traverse (\a -> state (runM (k a))) as
                             return (concat bss)))
#endif
#endif

get :: M Int
put :: Int -> M ()
amb :: [M a] -> M a

#ifdef SOLUTION
#if STEP == 1
get    = M (\s -> [(s, s)])
put s  = M (\_ -> [((), s)])
amb ms = M (\s -> concatMap (\m -> runM m s) ms)
#endif
#if STEP == 2
get    = M (\s -> ([s], s))
put s  = M (\_ -> ([()], s))
amb ms = M (runState (do ass <- traverse (\m -> state (runM m)) ms
                         return (concat ass)))
#endif
#endif
#endif

#if STEP == 3
type M = StateT Int []

runM :: M a -> Int -> [(a, Int)]
runM = runStateT

amb :: [M a] -> M a
amb []     =
#ifdef SOLUTION
             empty
#endif
amb (m:ms) =
#ifdef SOLUTION
             m <|> amb ms
-- or amb = Data.Foldable.asum
#endif
#endif

#if STEP == 4
type M = ListT (State Int)

runM :: M a -> Int -> ([a], Int)
runM = runState . runListT

amb :: [M a] -> M a
amb []     =
#ifdef SOLUTION
             empty
#endif
amb (m:ms) =
#ifdef SOLUTION
             m <|> amb ms
-- or amb = Data.Foldable.asum
#endif

law_m :: M ()
law_k :: M ()
law_l :: M ()
#ifdef SOLUTION
law_m = amb [append 1, append 1]
law_k = append 2
law_l = append 3
append :: Int -> M ()
append n = lift (modify (\s -> s * 10 + n))
#else
law_m = return ()
law_k = return ()
law_l = return ()
#endif
#endif
