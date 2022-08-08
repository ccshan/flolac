{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
#if STEP == 1
import Test.QuickCheck (quickCheckAll)
#else
import Test.QuickCheck (quickCheckAll, forAll, choose)
#endif
import Control.Monad (liftM, ap, replicateM)
import qualified Data.Map as M

newtype Prob a = MkProb {runProb :: [(a, Float)]}
  deriving Show

instance Functor Prob where fmap = liftM

instance Applicative Prob where pure = return; (<*>) = ap

instance Monad Prob where
  return a = MkProb [(a,1)]
  m >>= k  = MkProb (do (a,p) <- runProb m
                        (b,q) <- runProb (k a)
                        return (b,p*q))

coin :: Float -> Prob Int
coin p = MkProb [(1,p), (0,1-p)]

approx :: (Ord a) => Prob a -> Prob a -> Bool
-- ^Check if two distributions are approximately equal
approx m1 m2 = all small (M.unionWith (-) (M.fromListWith (+) (runProb m1))
                                          (M.fromListWith (+) (runProb m2)))
  where small p = abs p < 0.001

#if STEP >= 2
coalesce :: (Ord a) => Prob a -> Prob a
prop_coalesce1 p q   = approx (coalesce m) m where m = MkProb [(False,p), (True,q)]
prop_coalesce2 p q r = approx (coalesce m) m where m = MkProb [('a',p), ('b',q), ('c',r)]
prop_coalesce3 p q r = approx (coalesce (MkProb [('a',p), ('b',q), ('a',r)]))
                              (MkProb [('a',p+r), ('b',q)])
prop_coalesce4 p q r = approx (coalesce (MkProb [('a',p), ('b',q), ('b',r)]))
                              (MkProb [('a',p), ('b',q+r)])
prop_coalesce5 = forAll (choose (0,1)) (\p ->
                 approx (coalesce (count 3 p))
                 (MkProb [(0,(1-p)^3), (1,3*(1-p)^2*p), (2,3*(1-p)*p^2), (3,p^3)]))
#if STEP > 2 || STEP == 2 && defined(SOLUTION)
coalesce m = MkProb (M.toList (M.fromListWith (+) (runProb m)))
#endif
#endif

--------------------------------------------------------------------------------

die :: Prob Int
prop_die = approx die (MkProb [(1, 1/6),
                               (2, 1/6),
                               (3, 1/6),
                               (4, 1/6),
                               (5, 1/6),
                               (6, 1/6)])
#if STEP > 1 || STEP == 1 && defined(SOLUTION)
die = do
  x <- coin (1/6)
  if x == 1 then return 6 else do
    x <- coin (1/5)
    if x == 1 then return 5 else do
      x <- coin (1/4)
      if x == 1 then return 4 else do
        x <- coin (1/3)
        if x == 1 then return 3 else do
          x <- coin (1/2)
          if x == 1 then return 2 else return 1
#endif

dice :: Prob Int
dice = do d1 <- die
          d2 <- die
          return (d1 + d2)

count :: Int -> Float -> Prob Int
count n p = replicateM n (coin p) >>= return . sum

#if STEP >= 3
countL :: Int -> Float -> Prob Int
prop_countL = forAll (choose (0,1)) (\p ->
              forAll (choose (0,10)) (\n ->
              approx (count n p) (countL n p)))
countL 0 _ = return 0
#if STEP > 3 || STEP == 3 && defined(SOLUTION)
countL n p = do l <- coalesce (countL (n-1) p)
                r <- coin p
                return (l+r)
#else
countL n p = do l <- countL (n-1) p
                r <- coin p
                return (l+r)
#endif

countR :: Int -> Float -> Prob Int
prop_countR = forAll (choose (0,1)) (\p ->
              forAll (choose (0,10)) (\n ->
              approx (count n p) (countR n p)))
countR 0 _ = return 0
#if STEP > 4 || STEP == 4 && defined(SOLUTION)
countR n p = do l <- coin p
                r <- m
                return (l+r)
  where m = coalesce (countR (n-1) p)
#else
countR n p = do l <- coin p
                r <- countR (n-1) p
                return (l+r)
#endif
#endif

#if STEP >= 5
--------------------------------------------------------------------------------

two_coins :: Prob ((Bool, Bool), Bool)
two_coins = do
  x <- coin (1/2)
  y <- coin (1/2)
  return ((x == 1, y == 1),
          x == 1 && y == 1)

base :: Prob Bool
#ifdef SOLUTION
base = MkProb [(True, 1/4), (False, 3/4)]
#else
base = MkProb _
#endif

kernel :: Bool -> Prob (Bool, Bool)
kernel True  = return (True, True)
#ifdef SOLUTION
kernel False = MkProb [((True , False), 1/3),
                       ((False, True ), 1/3),
                       ((False, False), 1/3)]
#else
kernel False = MkProb _
#endif

prop_inference = approx two_coins (do both <- base
                                      pair <- kernel both
                                      return (pair, both))
#endif

return []
main = $quickCheckAll >>= print
