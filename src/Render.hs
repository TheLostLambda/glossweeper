module Render where

import Graphics.Gloss
import Model
import Util

padding = 3

tileColor = greyN 0.25
mineColor = dark $ dim red
flagColor = dark $ dim green

render :: Size -> Minesweeper -> Picture
render (sx,sy) (Game grid 0) = Pictures $ map (uncurry $ drawTile ts) positionGrid
  where win@(wx,wy) = (toNum sx, toNum sy)
        gs@(r,c) = gridSize grid
        ts = min ((wx - padding * (c + 1)) / c) ((wy - padding * (r + 1)) / r)
        positionGrid = zip (tilePositions win gs ts) (concat grid)

tilePositions :: (Float, Float) -> (Float, Float) -> Float -> [(Float, Float)]
tilePositions (wx,wy) (r,c) ts = [ (x - wx / 2, y - wy / 2)
                                 | rn <- [1..r]
                                 , cn <- [1..c]
                                 , let y = ts * rn + padding * rn - ts / 2
                                 , let x = ts * cn + padding * cn - ts / 2]

drawTile :: Float -> (Float, Float) -> Tile -> Picture
drawTile size (x,y) tile = Translate x y $ Color (tileToColor tile) $ squareSolid size

tileToColor :: Tile -> Color
tileToColor (Tile False _ False) = tileColor
tileToColor (Tile True True _) = mineColor
tileToColor (Tile True False _) = light tileColor
tileToColor (Tile _ _ True) = flagColor
