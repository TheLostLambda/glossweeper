module Model where

-- Write a custom show instance for Minesweeper
-- Try some form of pretty printing the Grid
data Minesweeper = Game { grid :: Grid
                        , size :: Window
                        , state :: Int
                        } deriving(Eq, Show, Read)

data Tile = Tile { revealed :: Bool
                 , mine :: Bool
                 , flag :: Bool
                 } deriving(Eq, Show, Read)

type Grid = [[Tile]]

type Size = (Int, Int)

type Window = (Float, Float)

initGame :: Size -> Size -> Minesweeper
initGame (wx,wy) (r,c) = Game (replicate r $ replicate c $ Tile False False False)
                              (fromIntegral wx, fromIntegral wy)
                              0
