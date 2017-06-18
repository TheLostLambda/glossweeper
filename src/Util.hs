module Util where

import Graphics.Gloss
import System.Random
import Data.List
import Model
import Config

gridSize :: Num a => Grid -> (a,a)
gridSize [] = (0,0)
gridSize grid = (toNum $ length grid, toNum $ length $ head grid)

getTile :: Grid -> (Int,Int) -> Tile
getTile grid (r,c) = (grid !! r) !! c

updateTile :: Grid -> (Int,Int) -> Tile -> Grid
updateTile grid (r,c) tile = take r grid ++ [take c (grid !! r) ++ [tile] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

placeMines :: Int -> Minesweeper -> Minesweeper
placeMines n ms@(Game grid _ rng _) = ms { grid = newGrid, rng = fst $ split rng }
  where (gr,gc) = gridSize grid :: (Int,Int)
        (rGen,cGen) = split rng
        mines = take n $ nub $ zip (randomRs (0, gr - 1) rGen) (randomRs (0, gc - 1) cGen)
        newGrid = foldr (\index grid -> updateTile grid index ((getTile grid index) { mine = True })) grid mines

getNeighbors :: Grid -> (Int,Int) -> [Tile]
getNeighbors grid (x,y) = map (getTile grid) neighbors
  where offsets = filter (/=(0,0)) [(x,y) | x <- [-1..1], y <- [-1..1]]
        neighbors = filter checkValid $ map (\(a,b) -> (x+a,y+b)) offsets
        checkValid (a,b) = a >= 0 && a < fst gameSize && b >= 0 && b < snd gameSize

neighboringMines :: Grid -> (Int,Int) -> Int
neighboringMines grid pos = length . filter mine $ getNeighbors grid pos

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
