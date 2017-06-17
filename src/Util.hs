module Util where

import Graphics.Gloss
import Model

gridSize :: Num a => Grid -> (a,a)
gridSize [] = (0,0)
gridSize grid = (toNum $ length grid, toNum $ length $ head grid)

updateTile :: Grid -> Tile -> Grid
updateTile = undefined

squareSolid :: Float -> Picture
squareSolid x = rectangleSolid x x

toNum :: (Num b, Integral a) => a -> b
toNum = fromIntegral
