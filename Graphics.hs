{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Graphics (norm, convert, thickLine, withKeyRepeat) where

import Test.QuickCheck (quickCheckAll, (==>))
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Geometry.Angle (radToDeg)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P

norm :: Point -> Float
norm (x,y) = sqrt (x*x + y*y)

(~=) :: (RealFrac a) => a -> a -> Bool
x ~= y = abs (x - y) < 1e-3

convert :: (RealFrac a) => a -> a -> a -> a -> a -> a
prop_convert_givens a1 b1 a2 b2 =
  a1 /= (a2 :: Float) ==> convert a1 b1 a2 b2 a1 ~= b1 &&
                          convert a1 b1 a2 b2 a2 ~= b2
prop_convert_middle a1 b1 a2 b2 =
  a1 /= (a2 :: Float) ==> convert a1 b1 a2 b2 ((a1 + a2) / 2) ~=
                                              ((b1 + b2) / 2)
convert a1 b1 a2 b2 a = (b1 * (a2 - a) + b2 * (a - a1)) / (a2 - a1)

thickLine :: Float -> Point -> Point -> Picture
thickLine w p q = uncurry translate q
                $ rotate (radToDeg (uncurry atan2 v))
                $ rectangleUpperSolid w (norm v)
  where v = p P.- q

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
