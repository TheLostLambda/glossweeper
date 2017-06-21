module Update where

import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.List
import Model
import Util
import Config

input :: Event -> Minesweeper -> Minesweeper
input (EventKey (MouseButton LeftButton) Down _ pos) ms@(Game grid _ _ 0)
  | isJust clicked && (mine . getTile ms . fromJust $ clicked) = ms { grid = revealedGrid , state = 1 }
  | isJust clicked = revealN ms (fromJust clicked)
  | otherwise = ms
  where clicked = decodeClick ms pos
        revealedGrid = Model.grid $ foldl reveal ms (indicies ms)
        reveal ms@(Game grid _ _ _) i = ms { grid = updateTile grid i ((getTile ms i) { revealed = True }) }
        revealN ms i
          | neighboringMines ms i == 0 = foldl revealN (reveal ms i) . filter (not . revealed . getTile ms) $ getNeighbors ms i
          | otherwise = reveal ms i

input (EventKey (MouseButton RightButton) Down _ pos) ms@(Game grid _ _ 0)
  | isJust clicked = ms { grid = updateTile grid index (tile { flag = newFlag }) }
  | otherwise = ms
  where tile = getTile ms index
        clicked = decodeClick ms pos
        index = fromJust clicked
        newFlag = not (flag tile)
input (EventKey (Char 'r') Down _ _) ms = placeMines mines $ resetGame ms
input (EventResize (w,h)) ms = ms { size = (toNum w, toNum h) }
input _ ms = ms

step :: Float -> Minesweeper -> Minesweeper
step _ ms@(Game grid _ _ 0)
  | length remaining > mines || length unflagged > 0 = ms
  | otherwise = ms { state = 2 }
  where remaining = filter (not . revealed) (concat grid)
        unflagged = filter (\x -> mine x && (not $ flag x)) (concat grid)
step _ ms = ms

decodeClick :: Minesweeper -> (Float, Float) -> Maybe (Int,Int)
decodeClick ms@(Game grid win _ _) (mx,my)
  | null row || null col = Nothing
  | otherwise = Just (head row, head col)
    where positions = tilePositions ms
          tileRadius = tileSize ms / 2
          rowRanges = zip [0..] $ nub $ map ((\y -> (y + tileRadius, y - tileRadius)) . snd) positions
          row = map fst $ filter (\(_,(u,l)) -> (my < u) && (my > l)) rowRanges
          colRanges = zip [0..] $ nub $ map ((\x -> (x + tileRadius, x - tileRadius)) . fst) positions
          col = map fst $ filter (\(_,(u,l)) -> (mx < u) && (mx > l)) colRanges
