module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 800
height = 600
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures
  [ color ballColor $ circleSolid 30
  , color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red 
    paddleColor = light (light blue)

main :: IO ()
main = display window background drawing 