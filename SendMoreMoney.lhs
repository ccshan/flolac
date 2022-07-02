%include preamble.lhs

\begin{code}
import Data.List (delete)

type Digit = Int
type Answer = String
type Body = [Digit] -> [Answer]

digit :: (Digit -> Body) -> Body
digit fun remain = concatMap (\digit -> fun digit (delete digit remain)) remain

check :: Bool -> Body -> Body
check bool body remain = if bool then body remain else []

add :: Digit -> Digit -> Digit -> Digit -> (Digit -> Body) -> Body
add in1 in2 out carry fun =
  let (carry', out') = divMod (in1 + in2 + carry) 10
  in check (out == out') (fun carry')

main =  print (
          (  digit (\d ->
             digit (\e ->
             digit (\y ->
             add d e y 0 (\carry ->
             digit (\n ->
             digit (\r ->
             add n r e carry (\carry ->
             digit (\o ->
             add e o n carry (\carry ->
             digit (\s ->
             check (s > 0) (
             digit (\m ->
             check (m > 0) (
             add s m o carry (\carry ->
             check (carry == m) (
              \ _  -> [                    show s  ++ show e  ++ show n  ++ show d
                     ++ "+"            ++  show m  ++ show o  ++ show r  ++ show e
                     ++ "=" ++ show m  ++  show o  ++ show n  ++ show e  ++ show y]))))))))))))))))
          [0..9])
\end{code}

Challenge: Generalize to TO + GO = OUT.\\
Use state to remember letters whose digits have been chosen.
