module Render where

import Graphics.Gloss
import Model
import Util

padding = 2

tileColor = greyN 0.25
mineColor = red
flagColor = green

render :: Size -> Minesweeper -> Picture
render (wx,wy) (Game grid 0) = Pictures $ map drawTile positions
  where (r,c) = gridSize grid
        ts = min ((wx - padding * (c + 1)) `div` c) ((wy - padding * (r + 1)) `div` r)
        -- Split this
        drawTile (x,y) = Translate (toNum x) (toNum y) $ Color tileColor $ squareSolid (toNum ts)
        -- Split this
        -- Zip positions to original tile
        positions = [ (x - wx `div` 2, y - wy `div` 2)
                    | rn <- [1..r]
                    , cn <- [1..c]
                    , let y = ts * rn + padding * rn - ts `div` 2
                    , let x = ts * cn + padding * cn - ts `div` 2]
