module Render where

import Graphics.Gloss
import Model
import Util
import Config

render :: Minesweeper -> Picture
render (Game grid win@(wx,wy) _ 0) = Pictures $ map (uncurry . drawTile $ tileSize win gs) positionGrid
  where gs@(r,c) = gridSize grid
        positionGrid = zip (tilePositions win gs) (concat grid)

drawTile :: Float -> (Float, Float) -> Tile -> Picture
drawTile size (x,y) tile = Translate x y $ Color (tileToColor tile) $ squareSolid size

tileToColor :: Tile -> Color
tileToColor (Tile False _ False) = tileColor
tileToColor (Tile True True _) = mineColor
tileToColor (Tile True False _) = light tileColor
tileToColor (Tile _ _ True) = flagColor
