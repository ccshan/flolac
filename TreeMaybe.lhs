%include preamble.lhs

\begin{comment}
\begin{code}
{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)

data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)
\end{code}
\end{comment}

\begin{code}
decTree :: Tree -> Maybe Tree
prop_dec1  =  decTree   (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
               == Just  (Branch (Leaf 2) (Branch (Leaf 4) (Leaf 1)))
prop_dec2  =  decTree   (Branch (Leaf 1) (Branch (Leaf 3) (Leaf 0)))
               == Nothing
\end{code}

\begin{solution}
\begin{code}
decTree (Leaf n)        =  if n > 0 then Just (Leaf (n-1)) else Nothing
decTree (Branch t1 t2)  =  case decTree t1 of
                             Nothing   ->  Nothing
                             Just t1'  ->  case decTree t2 of
                                             Nothing   ->  Nothing
                                             Just t2'  ->  Just (Branch t1' t2')
\end{code}
\end{solution}

\begin{code}
productTree :: Tree -> Int
\end{code}

\begin{comment}
\begin{code}
prop_product   =  productTree  (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2)))
                           ==  30
prop_product0  =  productTree  (Branch (Branch (Leaf 3) (Leaf 0)) undefined)
                           ==  0

productTree (Leaf n)        =  n
productTree (Branch t1 t2)  =  case productTree t1 of
                                 0 -> 0
                                 p1 -> case productTree t2 of
                                         0 -> 0
                                         p2 -> p1 * p2
\end{code}
\end{comment}

\begin{comment}
\begin{code}
return []
main = $quickCheckAll
\end{code}
\end{comment}
