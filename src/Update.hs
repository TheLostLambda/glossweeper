module Update where

import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.List
import Model
import Util
import Config

input :: Event -> Minesweeper -> Minesweeper
input (EventKey (MouseButton LeftButton) Down _ pos) ms@(Game grid _ _ _)
  | isJust clicked = ms { grid = updateTile grid index (tile { revealed = True }) }
  | otherwise = ms
  where tile = getTile grid index
        clicked = decodeClick ms pos
        index = fromJust clicked
input (EventKey (MouseButton RightButton) Down _ pos) ms@(Game grid _ _ _)
  | isJust clicked = ms { grid = updateTile grid index (tile { flag = newFlag }) }
  | otherwise = ms
  where tile = getTile grid index
        clicked = decodeClick ms pos
        index = fromJust clicked
        newFlag = if flag tile then False else True
input (EventKey (Char 'r') Down _ _) ms = placeMines mines $ resetGame ms
input (EventResize (w,h)) ms = ms { size = (toNum w, toNum h) }
input _ ms = ms

step :: Float -> Minesweeper -> Minesweeper
step _ ms = ms

decodeClick :: Minesweeper -> (Float, Float) -> Maybe (Int,Int)
decodeClick (Game grid win _ _) (mx,my)
  | null row || null col = Nothing
  | otherwise = Just (head row, head col)
    where positions = tilePositions win (gridSize grid)
          tileRadius = tileSize win (gridSize grid) / 2
          rowRanges = zip [0..] $ nub $ map ((\y -> (y + tileRadius, y - tileRadius)) . snd) positions
          row = map fst $ filter (\(_,(u,l)) -> (my < u) && (my > l)) rowRanges
          colRanges = zip [0..] $ nub $ map ((\x -> (x + tileRadius, x - tileRadius)) . fst) positions
          col = map fst $ filter (\(_,(u,l)) -> (mx < u) && (mx > l)) colRanges
