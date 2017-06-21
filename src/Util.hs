module Util where

import Graphics.Gloss
import System.Random
import Data.List
import Model
import Config

gridSize :: Num a => Grid -> (a,a)
gridSize [] = (0,0)
gridSize grid = (toNum $ length grid, toNum $ length $ head grid)

getTile :: Minesweeper -> (Int,Int) -> Tile
getTile (Game grid _ _ _) (r,c) = (grid !! r) !! c

updateTile :: Grid -> (Int,Int) -> Tile -> Grid
updateTile grid (r,c) tile = take r grid ++ [take c (grid !! r) ++ [tile] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

placeMines :: Int -> Minesweeper -> Minesweeper
placeMines n ms@(Game grid _ rng _) = ms { grid = newGrid, rng = fst $ split rng }
  where (gr,gc) = gridSize grid :: (Int,Int)
        (rGen,cGen) = split rng
        mines = take n $ nub $ zip (randomRs (0, gr - 1) rGen) (randomRs (0, gc - 1) cGen)
        newGrid = foldr (\index grid -> updateTile grid index ((getTile ms index) { mine = True })) grid mines

getNeighbors :: Minesweeper -> (Int,Int) -> [(Int,Int)]
getNeighbors ms (x,y) = filter checkValid $ map (\(a,b) -> (x+a,y+b)) offsets
  where offsets = filter (/=(0,0)) [(x,y) | x <- [-1..1], y <- [-1..1]]
        checkValid (a,b) = a >= 0 && a < fst gameSize && b >= 0 && b < snd gameSize

neighboringMines :: Minesweeper -> (Int,Int) -> Int
neighboringMines ms pos = length . filter mine . map (getTile ms) $ getNeighbors ms pos

squareSolid :: Float -> Picture
squareSolid x = rectangleSolid x x

toNum :: (Num b, Integral a) => a -> b
toNum = fromIntegral

tileSize :: Minesweeper -> Float
tileSize (Game grid (wx,wy) _ _) = min ((wx - padding * (c + 1)) / c) ((wy - padding * (r + 1)) / r)
  where (r,c) = gridSize grid

indicies :: Minesweeper -> [(Int,Int)]
indicies (Game grid _ _ _) = [(y,x) | y <- [0..(r - 1)], x <- [0..(c - 1)]]
  where (r,c) = gridSize grid

tilePositions :: Minesweeper -> [(Float, Float)]
tilePositions ms@(Game grid (wx,wy) _ _)  = [ (x - wx / 2, y - wy / 2)
                                            | rn <- [1..r]
                                            , cn <- [1..c]
                                            , let y = ts * rn + padding * rn - ts / 2
                                            , let x = ts * cn + padding * cn - ts / 2]
                                              where ts = tileSize ms
                                                    (r,c) = gridSize grid
