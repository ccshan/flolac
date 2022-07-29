{-# OPTIONS -W #-}

import Control.Monad (when)
import Data.Maybe (catMaybes)
import System.Random (randomRIO)
import Numeric (showGFloat)
import Graphics
import Graphics.Gloss
import Graphics.Gloss.Geometry.Line (intersectSegLine)
import Graphics.Gloss.Interface.IO.Game

xmin, xmax, ymin, ymax, size :: Float
xmin = -15
xmax =  35
ymin = - 5
ymax =  45
size = 300

convertPoint :: Point -> Point
convertPoint (x,y) = (convert xmin 0 xmax size x,
                      convert ymin (-size/2) ymax (size/2) y)

drawLinear :: Linear -> Picture
drawLinear l = Pictures
  $ [Line [convertPoint (xmin,0), convertPoint (xmax,0)],
     Line [convertPoint (0,ymin), convertPoint (0,ymax)]]
  ++ case catMaybes [ intersectSegLine p q (xmin, run l xmin) (xmax, run l xmax)
                    | (p,q) <- [((xmin,ymin),(xmin,ymax)),
                                ((xmax,ymin),(xmax,ymax)),
                                ((xmin,ymin),(xmax,ymin)),
                                ((xmin,ymax),(xmax,ymax))] ] of
       p:q:_ -> [Color red (thickLine 3 (convertPoint p) (convertPoint q))]
       _     -> []

drawLinearWithExamples :: [Point] -> Linear -> Picture
drawLinearWithExamples ps l = Pictures
  $ [drawLinear l]
  ++ [uncurry translate (convertPoint p) (circleSolid 4) | p <- ps]

draw :: Linear -> Picture
draw l = Pictures
  $ [drawLinearWithExamples examples l,
     translate (-size) 0         (scale 0.2  0.2  (text (show l))),
     translate (-size) (-size/2) (scale 0.15 0.15 (text (show (totalLoss l))))]

data Linear = Linear {intercept, slope :: Float}

instance Show Linear where
  showsPrec p (Linear i s) = showParen (p > 10)
                           $ showString "Linear "
                           . showGFloat (Just 3) i
                           . showChar ' '
                           . showGFloat (Just 3) s

instance Semigroup Linear where
  Linear i1 s1 <> Linear i2 s2 = Linear (i1+i2) (s1+s2)
instance Monoid Linear where
  mempty = Linear 0 0

run :: Linear -> Float -> Float
run l x = intercept l + slope l * x

loss :: Linear -> Point -> Float
loss l (x,y) = (run l x - y) ^ 2

examples :: [Point]
examples = [(0,26), (10,31), (20,40)]

totalLoss :: Linear -> Float
totalLoss l = sum (map (loss l) examples)

gradLoss :: Linear -> Point -> Linear
gradLoss l (x,y) = let backprop = 2 * (run l x - y)
                   in Linear backprop (backprop * x)

gradTotalLoss :: Linear -> Linear
gradTotalLoss l = mconcat (map (gradLoss l) examples)

step :: Linear -> Linear
step l = let grad = gradTotalLoss l
         in l{intercept = intercept l - 0.1   * intercept grad,
              slope     = slope     l - 0.001 * slope     grad}

handle :: Event -> Linear -> IO Linear
handle (EventKey k Down _ _) l = case k of
  Char '1' -> return (Linear 20 1)
  Char '2' -> return (Linear 10 2)
  Char 'r' -> Linear <$> randomRIO (10,40) <*> randomRIO (0,1.4)
  Char 'z' -> return l{intercept = intercept l - 0.1}
  Char 'x' -> return l{intercept = intercept l + 0.1}
  Char 'a' -> return l{slope     = slope     l - 0.1}
  Char 's' -> return l{slope     = slope     l + 0.1}
  SpecialKey KeySpace -> return (step l)
  _ -> return l
handle _ l = return l

edit :: Linear -> IO ()
edit l = withKeyRepeat 0.24 40 (playIO (InWindow "World" (round size, round size) (0, 0))
                                       white)
                       0
                       l
                       (return . draw)
                       handle
                       (const return)

main = edit (Linear 10 2)
