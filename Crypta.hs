{-# OPTIONS -W #-}

import qualified Data.Map as M
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

data S = S (M.Map Char Int) (M.Map Int Char) deriving (Show)
type M = StateT S []

digit :: Char -> M Int
digit c = do S c_i i_c <- get
             case M.lookup c c_i of
               Just i -> return i
               Nothing -> do i <- lift (M.keys (M.difference poss i_c))
                             put (S (M.insert c i c_i) (M.insert i c i_c))
                             return i
  where poss = M.fromDistinctAscList (zip [0..9] (repeat ()))

add :: Char -> Char -> Char -> Int -> M Int
add a b c carry = do x <- digit a
                     y <- digit b
                     z <- digit c
                     let (carry', z') = divMod (x + y + carry) 10
                     guard (z == z')
                     return carry'

adds :: [(Char, Char, Char)] -> Int -> M Int
adds [] carry = return carry
adds ((a,b,c):rest) carry = add a b c carry >>= adds rest

solve :: M ()
solve = do carry <- adds (reverse (zip3 "SEND" "MORE" "ONEY")) 0
           s <- digit 'S'
           guard (s > 0)
           m <- digit 'M'
           guard (m > 0 && carry == m)

main :: IO ()
main = print (execStateT solve (S M.empty M.empty))
