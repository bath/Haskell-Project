module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 800 -- actual window demension
height = 600 -- actual window demension
offset = 100 -- where does the window populate onto the computer screen

window :: Display
window = InWindow "Bouncing Betty" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [ball, walls,
                    mkPaddle green (280) 0 90]
  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  Top and side walls.
    wall :: Float -> Float -> Float -> Float -> Picture
    wall x y rot length =
      rotate rot $
        translate x y $
          color wallColor $
            rectangleSolid length 10

    wallColor = greyN 0.5
    walls = pictures [wall 0 290 0 790, wall 0 390 90 590, wall 0 (-390) 90 590]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Float -> Picture
    mkPaddle col x y rot = pictures
      [ rotate rot $ translate x y $ color col $ rectangleSolid 26 106
      , rotate rot $ translate x y $ color paddleColor $ rectangleSolid 20 100
      ]

    paddleColor = light (light white)

main :: IO ()
main = display window background drawing 