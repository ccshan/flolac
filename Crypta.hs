{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Data.List ((\\))
#if STEP == 3
import Control.Monad (liftM, ap)
#endif

type Digit = Int
#if STEP == 1
type Answer = String
type Chosen = [Digit]

digit :: Chosen -> [(Digit, Chosen)]
-- ^Try all digits not yet chosen
digit chosen = map (\d -> (d, d:chosen))
                   ([0..9] \\ chosen)
#endif
#if STEP == 2
type Answer = [(Char, Digit)]
type Chosen = [(Char, Digit)]

digit :: Char -> Chosen -> [(Digit, Chosen)]
-- ^Either recall this digit already chosen, or try all digits not yet chosen
#ifdef SOLUTION
digit c chosen =
  case lookup c chosen of
    Nothing -> map (\d -> (d, (c,d):chosen))
                   ([0..9] \\ map snd chosen)
    Just d  -> [(d, chosen)]
#endif
#endif
#if STEP == 3
type Answer = [(Char, Digit)]
type Chosen = [(Char, Digit)]
newtype M a = M {runM ::
#ifdef SOLUTION
                         Chosen -> [(a, Chosen)]}
#endif

instance Functor M where fmap = liftM

instance Applicative M where pure = return; (<*>) = ap

instance Monad M where
#ifdef SOLUTION
  return a = M (\s -> [(a, s)])
  m >>= k  = M (\s -> concatMap (\(a, s') -> runM (k a) s')
                                (runM m s))
#endif

digit :: Char -> M Digit
-- ^Either recall this digit already chosen, or try all digits not yet chosen
#ifdef SOLUTION
digit c = M (\chosen ->
  case lookup c chosen of
    Nothing -> map (\d -> (d, (c,d):chosen))
                   ([0..9] \\ map snd chosen)
    Just d  -> [(d, chosen)])
#endif
#endif

#if STEP <= 2
check :: Bool -> [()]
-- ^Return the empty list of answers if the given `Bool` is `False`
check bool = if bool then [()] else []
#endif
#if STEP >= 3
check :: Bool -> M ()
-- ^Return no answer if the given `Bool` is `False`
#ifdef SOLUTION
check bool = if bool then return () else M (\_ -> [])
#endif
#endif

#if STEP == 1
pythagorean :: [Answer]
-- ^Find nonzero digits that solve the equation X^2+Y^2=Z^2
pythagorean =
  concatMap
   (\(x,chosen) ->
     concatMap
      (\(y,chosen) ->
        concatMap
         (\(z,chosen) ->
           concatMap
            (\() ->
              [show x ++ "^2+" ++ show y ++ "^2=" ++ show z ++ "^2"])
            (check (x > 0 && y > 0 && x^2 + y^2 == z^2)))
         (digit chosen))
      (digit chosen))
   (digit [])
prop_pythagorean = pythagorean == ["3^2+4^2=5^2","4^2+3^2=5^2"]

add :: Digit -> Digit -> Digit -> Digit -> [Digit]
-- ^Check that `in1 + in2 + carry = 10 * carry' + out` and return `carry'`
add in1 in2 out carry =
  let (carry', out') = divMod (in1 + in2 + carry) 10
  in map (\() -> carry')
         (check (out == out'))

send_more_money :: [Answer]
-- ^Find digits that solve the equation SEND+MORE=MONEY
send_more_money =
  concatMap
   (\(d,chosen) ->
     concatMap
      (\(e,chosen) ->
        concatMap
         (\(y,chosen) ->
           concatMap
            (\carry ->
              concatMap
               (\(n,chosen) ->
                 concatMap
                  (\(r,chosen) ->
                    concatMap
                     (\carry ->
                       concatMap
                        (\(o,chosen) ->
                          concatMap
                           (\carry ->
                             concatMap
                              (\(s,chosen) ->
                                concatMap
                                 (\() ->
                                   concatMap
                                    (\(m,chosen) ->
                                      concatMap
                                       (\() ->
                                         concatMap
                                          (\carry ->
                                            concatMap
                                             (\() ->
                                               [          show s ++ show e ++ show n ++ show d ++ "+" ++
                                                          show m ++ show o ++ show r ++ show e ++ "=" ++
                                                show m ++ show o ++ show n ++ show e ++ show y])
                                             (check (carry == m)))
                                          (add s m o carry))
                                       (check (m > 0)))
                                    (digit chosen))
                                 (check (s > 0)))
                              (digit chosen))
                           (add e o n carry))
                        (digit chosen))
                     (add n r e carry))
                  (digit chosen))
               (digit chosen))
            (add d e y 0))
         (digit chosen))
      (digit chosen))
   (digit [])
prop_send_more_money = send_more_money == ["9567+1085=10652"]

to_go_out :: [Answer]
-- ^Find digits that solve the equation TO+GO=OUT
#ifdef SOLUTION
to_go_out =
  concatMap
   (\(o,chosen) ->
     concatMap
      (\() ->
        concatMap
         (\(t,chosen) ->
           concatMap
            (\() ->
              concatMap
               (\carry ->
                 concatMap
                  (\(g,chosen) ->
                    concatMap
                     (\() ->
                       concatMap
                        (\(u,chosen) ->
                          concatMap
                           (\carry ->
                             concatMap
                              (\() ->
                                [          show t ++ show o ++ "+" ++
                                           show g ++ show o ++ "=" ++
                                 show o ++ show u ++ show t])
                              (check (carry == o)))
                           (add t g u carry))
                        (digit chosen))
                     (check (g > 0)))
                  (digit chosen))
               (add o o t 0))
            (check (t > 0)))
         (digit chosen))
      (check (o > 0)))
   (digit [])
#endif
prop_to_go_out = to_go_out == ["21+81=102"]
#endif

#if STEP == 2
-- ^Find nonzero digits that solve the equation X^2+Y^2=Z^2
pythagorean =
  concatMap
   (\(x,chosen) ->
     concatMap
      (\(y,chosen) ->
        concatMap
         (\(z,chosen) ->
           map
            (\() -> chosen)
            (check (x > 0 && y > 0 && x^2 + y^2 == z^2)))
         (digit 'Z' chosen))
      (digit 'Y' chosen))
   (digit 'X' [])
prop_pythagorean = pythagorean
  == [[('Z',5),('Y',4),('X',3)], [('Z',5),('Y',3),('X',4)]]

add :: Char -> Char -> Char -> Digit -> Chosen -> [(Digit, Chosen)]
-- ^Check that `in1 + in2 + carry = 10 * carry' + out` and return `carry'`
add in1 in2 out carry chosen =
  concatMap
    (\(in1,chosen) ->
      concatMap
        (\(in2,chosen) ->
          concatMap
            (\(out,chosen) ->
              let (carry', out') = divMod (in1 + in2 + carry) 10
              in map (\() -> (carry',chosen))
                     (check (out == out')))
            (digit out chosen))
        (digit in2 chosen))
    (digit in1 chosen)

send_more_money :: [Answer]
-- ^Find digits that solve the equation SEND+MORE=MONEY
send_more_money =
  concatMap
   (\(carry,chosen) ->
     concatMap
      (\(carry,chosen) ->
        concatMap
         (\(carry,chosen) ->
           concatMap
            (\(carry,chosen) ->
              concatMap
               (\(s,chosen) ->
                 concatMap
                  (\(m,chosen) ->
                    map
                     (\() -> chosen)
                     (check (s > 0 && m > 0 && m == carry)))
                  (digit 'M' chosen))
               (digit 'S' chosen))
            (add 'S' 'M' 'O' carry chosen))
         (add 'E' 'O' 'N' carry chosen))
      (add 'N' 'R' 'E' carry chosen))
   (add 'D' 'E' 'Y' 0 [])
prop_send_more_money = send_more_money
  == [[('M',1),('S',9),('O',0),('R',8),('N',6),('Y',2),('E',5),('D',7)]]

to_go_out :: [Answer]
-- ^Find digits that solve the equation TO+GO=OUT
to_go_out =
  concatMap
   (\(carry,chosen) ->
     concatMap
      (\(carry,chosen) ->
        concatMap
         (\(t,chosen) ->
           concatMap
            (\(g,chosen) ->
              concatMap
               (\(o,chosen) ->
                 map
                  (\() -> chosen)
                  (check (t > 0 && g > 0 && o > 0 && o == carry)))
               (digit 'O' chosen))
            (digit 'G' chosen))
         (digit 'T' chosen))
      (add 'T' 'G' 'U' carry chosen))
   (add 'O' 'O' 'T' 0 [])
prop_to_go_out = to_go_out
  == [[('U',0),('G',8),('T',2),('O',1)]]
#endif

#if STEP == 3
pythagorean :: [Answer]
-- ^Find nonzero digits that solve the equation X^2+Y^2=Z^2
pythagorean = map snd (runM m [])
  where m = do x <- digit 'X'
               y <- digit 'Y'
               z <- digit 'Z'
               check (x > 0 && y > 0 && x^2 + y^2 == z^2)
prop_pythagorean = pythagorean
  == [[('Z',5),('Y',4),('X',3)], [('Z',5),('Y',3),('X',4)]]

add :: Char -> Char -> Char -> Digit -> M Digit
-- ^Check that `in1 + in2 + carry = 10 * carry' + out` and return `carry'`
#ifdef SOLUTION
add in1 in2 out carry =
  do in1 <- digit in1
     in2 <- digit in2
     out <- digit out
     let (carry', out') = divMod (in1 + in2 + carry) 10
     check (out == out')
     return carry'
#endif

send_more_money :: [Answer]
-- ^Find digits that solve the equation SEND+MORE=MONEY
send_more_money = map snd (runM m [])
  where m = do carry <- add 'D' 'E' 'Y' 0
               carry <- add 'N' 'R' 'E' carry
               carry <- add 'E' 'O' 'N' carry
               carry <- add 'S' 'M' 'O' carry
               s <- digit 'S'
               m <- digit 'M'
               check (s > 0 && m > 0 && m == carry)
prop_send_more_money = send_more_money
  == [[('M',1),('S',9),('O',0),('R',8),('N',6),('Y',2),('E',5),('D',7)]]

to_go_out :: [Answer]
-- ^Find digits that solve the equation TO+GO=OUT
to_go_out = map snd (runM m [])
  where m = do carry <- add 'O' 'O' 'T' 0
               carry <- add 'T' 'G' 'U' carry
               t <- digit 'T'
               g <- digit 'G'
               o <- digit 'O'
               check (t > 0 && g > 0 && o > 0 && o == carry)
prop_to_go_out = to_go_out
  == [[('U',0),('G',8),('T',2),('O',1)]]
#endif

return []
main = $quickCheckAll >>= print
