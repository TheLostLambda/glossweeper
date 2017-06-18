module Main where

import Graphics.Gloss
import Model
import Render
import Update
import Config

main :: IO ()
main = play (InWindow title screenSize (10,10))
            background
            fps
            (initGame screenSize gameSize)
            render
            input
            step
