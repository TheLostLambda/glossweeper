module Util where

import Graphics.Gloss
import Model
import Config

gridSize :: Num a => Grid -> (a,a)
gridSize [] = (0,0)
gridSize grid = (toNum $ length grid, toNum $ length $ head grid)

updateTile :: Grid -> (Int,Int) -> Tile -> Grid
updateTile grid (r,c) tile = take r grid ++ [take c (grid !! r) ++ [tile] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

squareSolid :: Float -> Picture
squareSolid x = rectangleSolid x x

toNum :: (Num b, Integral a) => a -> b
toNum = fromIntegral

tileSize :: Window -> (Float, Float) -> Float
tileSize (wx,wy) (r,c) = min ((wx - padding * (c + 1)) / c) ((wy - padding * (r + 1)) / r)

tilePositions :: Window -> (Float, Float) -> [(Float, Float)]
tilePositions win@(wx,wy) gs@(r,c) = [ (x - wx / 2, y - wy / 2)
                                        | rn <- [1..r]
                                        , cn <- [1..c]
                                        , let ts = tileSize win gs
                                        , let y = ts * rn + padding * rn - ts / 2
                                        , let x = ts * cn + padding * cn - ts / 2]
