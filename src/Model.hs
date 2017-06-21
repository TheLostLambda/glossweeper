module Model where

import System.Random

data Minesweeper = Game { grid :: Grid
                        , size :: Window
                        , rng :: StdGen
                        , state :: Int
                        } deriving(Show, Read)

data Tile = Tile { revealed :: Bool
                 , mine :: Bool
                 , flag :: Bool
                 } deriving(Eq, Show, Read)

type Grid = [[Tile]]

type Size = (Int, Int)

type Window = (Float, Float)

initGame :: Size -> Size -> StdGen -> Minesweeper
initGame (wx,wy) (r,c) rng = Game (replicate r $ replicate c $ Tile False False False)
                                  (fromIntegral wx, fromIntegral wy)
                                  rng
                                  0

resetGame :: Minesweeper -> Minesweeper
resetGame ms@(Game grid _ _ _) = ms { grid = map (map (\x -> Tile False False False)) grid, state = 0}
