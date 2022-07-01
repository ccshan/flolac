%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import qualified Data.Set as S

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

sumTree  :: Tree -> Int

prop_sum  =  sumTree   (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
                   ==  30
\end{code}
\end{comment}

\subsection{Accumulator passing}

\begin{spec}
result := 0

sumTree (Leaf n)        =  result := result + n;
                           result
sumTree (Branch t1 t2)  =  sumTree t1;
                           sumTree t2
\end{spec}

\begin{code}
sumTree' :: Tree -> Int -> Int
\end{code}

\begin{solution}
\begin{code}
sumTree t = sumTree' t 0

sumTree' (Leaf n) result        =  result + n
sumTree' (Branch t1 t2) result  =  let  result1 = sumTree' t1 result
                                        result2 = sumTree' t2 result1
                                   in result2
\end{code}
\end{solution}

\subsection{State threading}

\begin{code}
relabel :: Tree -> Tree
prop_relabel =  relabel (  Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
                      ==   Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
\end{code}

\begin{spec}
next := 0
relabel (Leaf _)        =  next := next + 1;
                           Leaf next
relabel (Branch t1 t2)  =  Branch (relabel t1) (relabel t2)
\end{spec}

\begin{code}
relabel' :: Tree -> Int -> (Tree, Int)
\end{code}

\begin{solution}
\begin{code}
relabel t = let (t', _) = relabel' t 0 in t'

relabel' (Leaf _)        next =  (Leaf (next+1), next+1)
relabel' (Branch t1 t2)  next =  let  (t1', next1) = relabel' t1 next
                                      (t2', next2) = relabel' t2 next1
                                 in (Branch t1' t2', next2)
\end{code}
\end{solution}

\begin{code}
unique :: Tree -> Bool
prop_unique1 =  unique (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2))) == True
prop_unique2 =  unique (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 3))) == False
\end{code}

\begin{spec}
import qualified Data.Set as S

seen := S.empty
unique (Leaf n)        =  if S.member n seen then False
                          else seen := S.insert n seen; True
unique (Branch t1 t2)  =  unique t1 && unique t2
\end{spec}

\begin{code}
unique' :: Tree -> S.Set Int -> (Bool, S.Set Int)

unique'' :: Tree -> (Bool, S.Set Int)
\end{code}

\begin{solution}
\begin{code}
unique t = let (u, _) = unique' t S.empty in u

unique' (Leaf n)        seen  =  if S.member n seen then (False, seen)
                                 else (True, S.insert n seen)
unique' (Branch t1 t2)  seen  =  let  (u1, seen1) = unique' t1 seen
                                      (u2, seen2) = unique' t2 seen1
                                 in (u1 && u2, seen2)

-- unique t = let (u, _) = unique'' t in u

unique'' (Leaf n)        =  (True, S.singleton n)
unique'' (Branch t1 t2)  =  let  (u1, seen1) = unique'' t1
                                 (u2, seen2) = unique'' t2
                            in (u1 && u2 && S.disjoint seen1 seen2, S.union seen1 seen2)
\end{code}
\end{solution}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
