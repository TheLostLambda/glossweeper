module Util where

import Graphics.Gloss
import Model

gridSize :: Grid -> Size
gridSize [] = (0,0)
gridSize grid = (length grid, length $ head grid)

squareSolid :: Float -> Picture
squareSolid x = rectangleSolid x x

toNum :: (Num b, Integral a) => a -> b
toNum = fromIntegral
