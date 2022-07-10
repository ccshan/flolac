{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Data.List ((\\))

type Digit = Int
type Body = State -> [Answer]
#if STEP == 1
type Answer = String
type State = [Digit]

digit :: (Digit -> Body) -> Body
digit fun chosen = concatMap (\d -> fun d (d:chosen))
                             ([0..9] \\ chosen)
#endif
#if STEP == 2
type Answer = [(Char,Digit)]
type State = [(Char,Digit)]

digit :: Char -> (Digit -> Body) -> Body
#ifdef SOLUTION
digit c fun chosen =
  case lookup c chosen of
    Nothing -> concatMap (\d -> fun d ((c,d):chosen))
                         ([0..9] \\ map snd chosen)
    Just d  -> fun d chosen
#endif
#endif

check :: Bool -> Body -> Body
check bool body chosen = if bool then body chosen else []

#if STEP == 1
prop_pythagorean =
  (digit (\x ->
   digit (\y ->
   digit (\z ->
   check (x > 0 && y > 0 && x^2 + y^2 == z^2) (
    \_ -> [show x ++ "^2+" ++ show y ++ "^2=" ++ show z ++ "^2"]))))) []
  == ["3^2+4^2=5^2","4^2+3^2=5^2"]

add :: Digit -> Digit -> Digit -> Digit -> (Digit -> Body) -> Body
add in1 in2 out carry fun =
  let (carry', out') = divMod (in1 + in2 + carry) 10
  in check (out == out') (fun carry')

prop_send_more_money =
  (digit (\d ->
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
    \_ -> [                  show s ++ show e ++ show n ++ show d
         ++ "+"           ++ show m ++ show o ++ show r ++ show e
         ++ "=" ++ show m ++ show o ++ show n ++ show e ++ show y])))))))))))))))) []
  == ["9567+1085=10652"]
#endif

#if STEP == 2
prop_pythagorean =
  (digit 'X' (\x ->
   digit 'Y' (\y ->
   digit 'Z' (\z ->
   check (x > 0 && y > 0 && x^2 + y^2 == z^2) (\chosen -> [chosen]))))) []
  == [[('Z',5),('Y',4),('X',3)], [('Z',5),('Y',3),('X',4)]]

add :: Char -> Char -> Char -> Digit -> (Digit -> Body) -> Body
add in1 in2 out carry fun =
  digit in1 (\in1 ->
  digit in2 (\in2 ->
  digit out (\out ->
  let (carry', out') = divMod (in1 + in2 + carry) 10
  in check (out == out') (fun carry'))))

prop_send_more_money =
  (add 'D' 'E' 'Y' 0     (\carry ->
   add 'N' 'R' 'E' carry (\carry ->
   add 'E' 'O' 'N' carry (\carry ->
   add 'S' 'M' 'O' carry (\carry ->
   digit 'S' (\s ->
   digit 'M' (\m ->
   check (s > 0 && m > 0 && m == carry) (\chosen -> [chosen])))))))) []
  == [[('M',1),('S',9),('O',0),('R',8),('N',6),('Y',2),('E',5),('D',7)]]

prop_to_go_out =
  (add 'O' 'O' 'T' 0     (\carry ->
   add 'T' 'G' 'U' carry (\carry ->
   digit 'T' (\t ->
   digit 'G' (\g ->
   digit 'O' (\o ->
   check (t > 0 && g > 0 && o == carry) (\chosen -> [chosen]))))))) []
  == [[('U',0),('G',8),('T',2),('O',1)]]
#endif

return []
main = $quickCheckAll >>= print
