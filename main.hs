module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 800
height = 600
offset = 600

window :: Display
window = InWindow "Bouncing Betty" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [ball, walls,
                    mkPaddle rose 120 (-20),
                    mkPaddle orange (-120) 40]
  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom top side walls.
    wall :: Float -> Float -> Float -> Picture
    wall x y rot =
      rotate rot $
        translate x y $
          color wallColor $
            rectangleSolid 310 10

    wallColor = greyN 0.5
    walls = pictures [wall 0 150 0, wall 0 (-150) 0, wall 0 150 90 ]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

main :: IO ()
main = display window background drawing 