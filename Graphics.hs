{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Graphics (norm, convert, thickLine,
                 drawResults, drawWeights, drawNeurons,
                 withKeyRepeat) where

import Test.QuickCheck (quickCheckAll, (==>))
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Geometry.Angle (radToDeg)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import qualified Data.Map as M
import Diff5 (Params, Name, Inertia(Inertia))

norm :: Point -> Float
norm (x,y) = sqrt (x*x + y*y)

(~=) :: (RealFrac a) => a -> a -> Bool
x ~= y = abs (x - y) < 0.01

convert :: (RealFrac a) => a -> a -> a -> a -> a -> a
prop_convert_givens a1 b1 a2 b2 =
  a1 /= (a2 :: Float) ==> convert a1 b1 a2 b2 a1 ~= b1 &&
                          convert a1 b1 a2 b2 a2 ~= b2
prop_convert_middle a1 b1 a2 b2 =
  a1 /= (a2 :: Float) ==> convert a1 b1 a2 b2 ((a1 + a2) / 2) ~=
                                              ((b1 + b2) / 2)
convert a1 b1 a2 b2 a = (b1 * (a2 - a) + b2 * (a - a1)) / (a2 - a1)

thickLine :: Float -> Point -> Point -> Picture
-- ^A line segment between the two given points with the given width
thickLine w p q = uncurry translate p
                $ rotate (radToDeg (uncurry atan2 d))
                $ rectangleUpperSolid w (norm d)
  where d = q P.- p

placeNearEnd :: Point -> Point -> Picture -> Picture
-- ^Rotate and place the given sprite near the end of the straight line
--  from the first given point to the second
prop_placeNearEnd1 = (placeNearEnd (100,100) (200,100) $ text "Picture")
                     == (translate 155 100 $ rotate 0 $ text "Picture")
prop_placeNearEnd2 = (placeNearEnd (100,150) (100,50) $ text "Picture")
                     == (translate 100 95 $ rotate 90 $ text "Picture")
placeNearEnd p q = uncurry translate (q P.- (45 / norm d) P.* d)
                 . rotate (radToDeg (negate (uncurry (flip atan2) d)))
  where d = q P.- p

weightToColor :: Float -> Color
-- ^Compute the color that depicts the given weight
weightToColor w = if w > 0 then orange else azure

weightToWidth :: Float -> Float
-- ^Compute the line width that depicts the given weight
prop_weightToWidth1 = weightToWidth   1000  == 25
prop_weightToWidth2 = weightToWidth (-1000) == 25
weightToWidth w = abs (recip (1 + exp (-w)) - 0.5) * 50

drawWeight :: Float -> Point -> Point -> Picture
-- ^Draw a weight w of a network connection
--  from the first given point to the second
minusSign = color white (rectangleSolid 20 5)
plusSign = pictures [minusSign, color white (rectangleSolid 5 20)]
prop_drawWeight1 = drawWeight 1000 (100,100) (200,100)
                   == pictures [color orange (thickLine 25 (100,100) (200,100)),
                                translate 155 100 (rotate 0 plusSign)]
prop_drawWeight2 = drawWeight (-1000) (100,150) (100,50)
                   == pictures [color azure (thickLine 25 (100,150) (100,50)),
                                translate 100 95 (rotate 90 minusSign)]
drawWeight w p q =
  pictures [color (weightToColor w) (thickLine (weightToWidth w) p q),
            placeNearEnd p q (if w > 0 then plusSign else minusSign)]

drawNeuron :: Float -> Float -> Picture
-- ^Draw a neuron with the given size and activation level
drawNeuron size level
  | level > 0 = pictures [color (mixColors level (1-level) orange white)
                                (circleSolid (size/2)),
                          color white (rectangleSolid (2/3*size) (1/6*size)),
                          color white (rectangleSolid (1/6*size) (2/3*size))]
  | otherwise = pictures [color (mixColors (-level) (1+level) azure white)
                                (circleSolid (size/2)),
                          color white (rectangleSolid (2/3*size) (1/6*size))]

drawResults :: (Float -> Float -> Float) -> Picture
-- ^Draw an array of output activation levels for different inputs x and y
--  (The array is located in the rectangle (-120,-120) (105,105))
drawResults f =
  pictures $ [ translate (x * 100) (y * 100) (drawNeuron 10 (f x y))
             | x <- levels, y <- levels ]
           ++ [ translate (x * 100) (-115) (drawNeuron 10 x) | x <- levels ]
           ++ [ translate (-115) (y * 100) (drawNeuron 10 y) | y <- levels ]
  where levels = map (/10) [-10..10]

drawWeights :: Params -> [(Name, Point, Point)] -> Picture
-- ^Draw weights of network connections, as laid out in the given list
drawWeights env layouts =
  pictures [ drawWeight (let Inertia r _ = env M.! name in r) p q
           | (name,p,q) <- layouts ]

drawNeurons :: [(String, Point)] -> Picture
-- ^Draw neurons as laid out in the given list
drawNeurons layouts =
  pictures [ uncurry translate p
           $ pictures [color white (circleSolid 35),
                       color black (thickCircle 35 1),
                       translate (-15) (-15) (scale 0.3 0.3 (text name))]
           | (name,p) <- layouts ]

data KeyRepeat world = KeyRepeat (Maybe (Float, Key, Modifiers, Point)) world
type GlossIO = forall world. Int ->
                             world ->
                             (world -> IO Picture) ->
                             (Event -> world -> IO world) ->
                             (Float -> world -> IO world) -> IO ()

withKeyRepeat :: Float -> Int -> GlossIO -> GlossIO
withKeyRepeat delay rate glossIO
              simResolution worldStart worldToPicture worldHandleEvent worldAdvance
  = glossIO (max simResolution rate)
            (KeyRepeat Nothing worldStart)
            (\(KeyRepeat _ world) -> worldToPicture world)
            (\event (KeyRepeat pressed world) ->
               KeyRepeat (handle event pressed)
                 <$> worldHandleEvent event world)
            (\dt (KeyRepeat pressed world) -> case pressed of
               Just (t0, k, mods, p) ->
                 let loop t w | t <= 0 = worldHandleEvent (EventKey k Down mods p) w
                                         >>= loop (t + recip (fromIntegral rate))
                              | otherwise = KeyRepeat (Just (t, k, mods, p))
                                            <$> worldAdvance dt w
                 in loop (t0 - dt) world
               Nothing -> KeyRepeat pressed <$> worldAdvance dt world)
  where handle (EventKey (MouseButton _) _ _ _) pressed = pressed
        handle (EventKey (SpecialKey KeyShiftL) _ _ _) pressed = pressed
        handle (EventKey (SpecialKey KeyShiftR) _ _ _) pressed = pressed
        handle (EventKey (SpecialKey KeyCtrlL) _ _ _) pressed = pressed
        handle (EventKey (SpecialKey KeyCtrlR) _ _ _) pressed = pressed
        handle (EventKey (SpecialKey KeyAltL) _ _ _) pressed = pressed
        handle (EventKey (SpecialKey KeyAltR) _ _ _) pressed = pressed
        handle (EventKey k Down mods p) _ = Just (delay, k, mods, p)
        handle (EventKey k Up _ _) (Just (_, k', _, _)) | k == k' = Nothing
        handle (EventMotion p) (Just (t, k, mods, _)) = Just (t, k, mods, p)
        handle _ pressed = pressed

return []
main = $quickCheckAll >>= print
