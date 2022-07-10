%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import qualified Data.Set as S

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)
\end{code}
\end{comment}

\begin{code}
eqSet :: [Int] -> [Int] -> Bool
eqSet s1 s2 = S.fromList s1 == S.fromList s2

blackjack :: Tree -> [Int]
prop_blackjack1  =  eqSet  (blackjack (Branch  (Branch (Leaf 10) (Leaf 9))
                                               (Branch (Leaf 12) (Leaf 11))))
                           [0,9,10,11,12,19,20,21]
prop_blackjack2  =  eqSet  (blackjack (Branch  (Branch (Leaf 11) (Leaf (-1)))
                                               (Leaf 11)))
                           [-1,0,10,11,21]
prop_blackjack3  =  eqSet  (blackjack (Branch  (Leaf 11)
                                               (Branch (Leaf 11) (Leaf (-1)))))
                           [-1,0,10,11]

blackjack t = blackjack' t 0

blackjack' :: Tree -> Int -> [Int]
\end{code}

\begin{spec}
blackjack' (Leaf n)        total  =  if total + n > 21 then total
                                     else amb [total, total + n]
blackjack' (Branch t1 t2)  total  =  blackjack' t2 (blackjack' t1 total)
\end{spec}

\begin{solution}
\begin{code}
blackjack' (Leaf n)        total  =  if total + n > 21 then [total]
                                     else [total, total + n]
blackjack' (Branch t1 t2)  total  =  concatMap (blackjack' t2) (blackjack' t1 total)
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
