module Main where

import Graphics.Gloss
import Model
import Render
import Update

title :: String
title = "Glossweeper"

screenSize :: (Int,Int)
screenSize = (600, 600)

gameSize :: (Int,Int)
gameSize = (10,10)

background :: Color
background = greyN 0.75

fps :: Int
fps = 60

main :: IO ()
--main = undefined
main = play (InWindow title screenSize (10,10))
            background
            fps
            (initGame gameSize)
            (render screenSize)
            input
            step
