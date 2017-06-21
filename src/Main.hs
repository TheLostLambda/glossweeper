module Main where

import Graphics.Gloss
import System.Random
import Model
import Render
import Update
import Config
import Util

main :: IO ()
main = do
  rng <- getStdGen
  play (InWindow title screenSize (10,10))
       background
       fps
       (placeMines mines $ initGame screenSize gameSize rng)
       render
       input
       step
