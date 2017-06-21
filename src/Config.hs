module Config where

import Graphics.Gloss

title :: String
title = "Glossweeper"

screenSize :: (Int,Int)
screenSize = (900, 900)

gameSize :: (Int,Int)
gameSize = (10,10)

background :: Color
background = greyN 0.75

fps :: Int
fps = 60

padding :: Float
padding = 2

tileColor :: Color
tileColor = greyN 0.25

mineColor :: Color
mineColor = dark $ dim red

flagColor :: Color
flagColor = dark $ dim green

overlayColor :: Color
overlayColor = makeColor 0 0 0 0.7

mines :: Int
mines = 15
