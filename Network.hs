{-# OPTIONS -W #-}

import Graphics (drawWeights, drawNeurons, drawResults, withKeyRepeat)
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import Diff5 (Params, Inertia(Inertia), eval, randomParams,
              network, networkLoss,
              optimize)
import Control.Monad.Identity (runIdentity)

drawNetwork :: Params -> Picture
drawNetwork env =
  pictures [translate (-390) (-285/2) net, translate 255 (15/2) res]
  where net = pictures [drawWeights env [("a0", (250, 15), (250, 85)),
                                         ("a1", ( 50, 85), (250, 85)),
                                         ("a2", ( 50,235), (250, 85)),
                                         ("b0", (250,165), (250,235)),
                                         ("b1", ( 50, 85), (250,235)),
                                         ("b2", ( 50,235), (250,235)),
                                         ("c0", (450, 90), (450,160)),
                                         ("c1", (250, 85), (450,160)),
                                         ("c2", (250,235), (450,160))],
                        drawNeurons [("x", ( 50, 85)),
                                     ("y", ( 50,235)),
                                     ("a", (250, 85)),
                                     ("b", (250,235)),
                                     ("c", (450,160))]]
        res = drawResults (\x y -> runIdentity
                                 $ eval network
                                 $ M.insert "x" x
                                 $ M.insert "y" y
                                 $ M.map (\(Inertia v _) -> v)
                                 $ env)

handleNetwork :: Event -> Params -> IO Params
handleNetwork (EventKey k Down _ _) env = case k of
  Char 'r' -> randomParams networkLoss
  SpecialKey KeySpace -> optimize 0 networkLoss env
  SpecialKey KeyEnter -> optimize 999 networkLoss env
  _ -> return env
handleNetwork _ env = return env

main = do
  env <- randomParams networkLoss
  withKeyRepeat 0.24 40 (playIO (InWindow "World" (780,285) (0,0)) white)
                40
                env
                (return . drawNetwork)
                handleNetwork
                (const (optimize 0 networkLoss))
