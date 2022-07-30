{-# OPTIONS -W #-}

import Graphics (drawWeights, drawNeurons, drawResults, withKeyRepeat)
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import Diff5 (Params, Inertia(Inertia), eval, randomParams,
              perceptron, perceptronLoss,
              optimize)
import Control.Monad.Identity (runIdentity)

drawPerceptron :: Params -> Picture
drawPerceptron env =
  pictures [translate (-290) (-285/2) net, translate 155 (15/2) res]
  where net = pictures [drawWeights env [("a0", (250, 90), (250,160)),
                                         ("a1", ( 50, 85), (250,160)),
                                         ("a2", ( 50,235), (250,160))],
                        drawNeurons [("x", ( 50, 85)),
                                     ("y", ( 50,235)),
                                     ("a", (250,160))]]
        res = drawResults (\x y -> runIdentity
                                 $ eval perceptron
                                 $ M.insert "x" x
                                 $ M.insert "y" y
                                 $ M.map (\(Inertia v _) -> v)
                                 $ env)

adjustPerceptron :: Float -> Float -> Float -> Params -> Params
adjustPerceptron d0 d1 d2
  = M.intersectionWith adjust (M.fromList [("a0",d0), ("a1",d1), ("a2",d2)])
  where adjust d (Inertia v _) = Inertia (v+d) d

handlePerceptron :: Event -> Params -> IO Params
handlePerceptron (EventKey k Down _ _) env = case k of
  Char 'r' -> randomParams perceptronLoss
  Char 'z' -> return (adjustPerceptron (-1) 0 0 env)
  Char 'x' -> return (adjustPerceptron   1  0 0 env)
  Char 'a' -> return (adjustPerceptron 0 (-1) 0 env)
  Char 's' -> return (adjustPerceptron 0   1  0 env)
  Char 'q' -> return (adjustPerceptron 0 0 (-1) env)
  Char 'w' -> return (adjustPerceptron 0 0   1  env)
  SpecialKey KeySpace -> optimize 0 perceptronLoss env
  SpecialKey KeyEnter -> optimize 999 perceptronLoss env
  _ -> return env
handlePerceptron _ env = return env

main = do
  env <- randomParams perceptronLoss
  withKeyRepeat 0.24 40 (playIO (InWindow "World" (580,285) (0,0)) white)
                0
                env
                (return . drawPerceptron)
                handlePerceptron
                (const return)
