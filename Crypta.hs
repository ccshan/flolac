{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Data.List ((\\))

type Digit = Int
type Body = Chosen -> [Answer]
#if STEP == 1
type Answer = String
type Chosen = [Digit]

digit :: (Digit -> Body) -> Body
-- ^Try all digits not yet chosen
digit fun chosen = concatMap (\d -> fun d (d:chosen))
                             ([0..9] \\ chosen)
#endif
#if STEP == 2
type Answer = [(Char,Digit)]
type Chosen = [(Char,Digit)]

digit :: Char -> (Digit -> Body) -> Body
-- ^Either recall this digit already chosen, or try all digits not yet chosen
#ifdef SOLUTION
digit c fun chosen =
  case lookup c chosen of
    Nothing -> concatMap (\d -> fun d ((c,d):chosen))
                         ([0..9] \\ map snd chosen)
    Just d  -> fun d chosen
#endif
#endif

check :: Bool -> Body -> Body
-- ^Return the empty list of answers if the given `Bool` is `False`
check bool body chosen = if bool then body chosen else []

#if STEP == 1
pythagorean :: [Answer]
-- ^Find nonzero digits that solve the equation X^2+Y^2=Z^2
pythagorean =
  (digit (\x ->
   digit (\y ->
   digit (\z ->
   check (x > 0 && y > 0 && x^2 + y^2 == z^2) (
    \_ -> [show x ++ "^2+" ++ show y ++ "^2=" ++ show z ++ "^2"]))))) []
prop_pythagorean = pythagorean == ["3^2+4^2=5^2","4^2+3^2=5^2"]

add :: Digit -> Digit -> Digit -> Digit -> (Digit -> Body) -> Body
-- ^Check that `in1 + in2 + carry = 10 * carry' + out`
-- and pass `carry'` to continuation
add in1 in2 out carry fun =
  let (carry', out') = divMod (in1 + in2 + carry) 10
  in check (out == out') (fun carry')

send_more_money :: [Answer]
-- ^Find digits that solve the equation SEND+MORE=MONEY
send_more_money =
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
prop_send_more_money = send_more_money == ["9567+1085=10652"]

to_go_out :: [Answer]
-- ^Find digits that solve the equation TO+GO=OUT
#ifdef SOLUTION
to_go_out =
  (digit (\o ->
   check (o > 0) (
   digit (\t ->
   check (t > 0) (
   add o o t 0 (\carry ->
   digit (\g ->
   check (g > 0) (
   digit (\u ->
   add t g u carry (\carry ->
   check (carry == o) (
    \_ -> [                  show t ++ show o
         ++ "+"           ++ show g ++ show o
         ++ "=" ++ show o ++ show u ++ show t]))))))))))) []
#endif
prop_to_go_out = to_go_out == ["21+81=102"]
#endif

#if STEP == 2
pythagorean :: [Answer]
-- ^Find nonzero digits that solve the equation X^2+Y^2=Z^2
pythagorean =
  (digit 'X' (\x ->
   digit 'Y' (\y ->
   digit 'Z' (\z ->
   check (x > 0 && y > 0 && x^2 + y^2 == z^2) (\chosen -> [chosen]))))) []
prop_pythagorean = pythagorean
  == [[('Z',5),('Y',4),('X',3)], [('Z',5),('Y',3),('X',4)]]

add :: Char -> Char -> Char -> Digit -> (Digit -> Body) -> Body
-- ^Check that `in1 + in2 + carry = 10 * carry' + out`
-- and pass `carry'` to continuation
add in1 in2 out carry fun =
  digit in1 (\in1 ->
  digit in2 (\in2 ->
  digit out (\out ->
  let (carry', out') = divMod (in1 + in2 + carry) 10
  in check (out == out') (fun carry'))))

send_more_money :: [Answer]
-- ^Find digits that solve the equation SEND+MORE=MONEY
send_more_money =
  (add 'D' 'E' 'Y' 0     (\carry ->
   add 'N' 'R' 'E' carry (\carry ->
   add 'E' 'O' 'N' carry (\carry ->
   add 'S' 'M' 'O' carry (\carry ->
   digit 'S' (\s ->
   digit 'M' (\m ->
   check (s > 0 && m > 0 && m == carry) (\chosen -> [chosen])))))))) []
prop_send_more_money = send_more_money
  == [[('M',1),('S',9),('O',0),('R',8),('N',6),('Y',2),('E',5),('D',7)]]

to_go_out :: [Answer]
-- ^Find digits that solve the equation TO+GO=OUT
to_go_out =
  (add 'O' 'O' 'T' 0     (\carry ->
   add 'T' 'G' 'U' carry (\carry ->
   digit 'T' (\t ->
   digit 'G' (\g ->
   digit 'O' (\o ->
   check (t > 0 && g > 0 && o == carry) (\chosen -> [chosen]))))))) []
prop_to_go_out = to_go_out
  == [[('U',0),('G',8),('T',2),('O',1)]]
#endif

return []
main = $quickCheckAll >>= print
