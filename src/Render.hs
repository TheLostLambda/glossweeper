module Render where

import Graphics.Gloss
import Model
import Util
import Config

render :: Minesweeper -> Picture
render ms@(Game grid _ _ 0) = Pictures $ map (drawTile ms) (indicies ms)
render ms@(Game grid (wx,wy) _ 1) = Pictures $ map (drawTile ms) (indicies ms) ++ drawMessage ms 310 "You Lose!"
render ms@(Game grid (wx,wy) _ 2) = Pictures $ map (drawTile ms) (indicies ms) ++ drawMessage ms 280 "You Win!"

drawMessage :: Minesweeper -> Float -> String -> [Picture]
drawMessage (Game _ (wx,wy) _ _) off txt = [Color overlayColor $ rectangleSolid wx wy, Color white $ Translate (-s * off) 0 $ Scale s s $ text txt]
  where s = min wx wy / 750

drawTile :: Minesweeper -> (Int, Int) -> Picture
drawTile ms@(Game grid (wx,wy) _ _) index@(r,c) = Translate x y $ Pictures renderLst
  where (x,y) = tilePositions ms !! (r * w + c)
        (h,w) = gridSize grid
        tile = getTile ms index
        mines = neighboringMines ms index
        s = size / 150
        (tx,ty) = (-37.5 * s, -50 * s)
        numDisplay = revealed tile && (not $ mine tile) && mines > 0
        renderLst = [ Color (tileToColor tile) $ squareSolid size
                    , if numDisplay
                        then (Translate tx ty $ Scale s s $ text (show mines))
                        else Blank]
        size = tileSize ms

tileToColor :: Tile -> Color
tileToColor (Tile False _ False) = tileColor
tileToColor (Tile True True _) = mineColor
tileToColor (Tile True False _) = light tileColor
tileToColor (Tile _ _ True) = flagColor
