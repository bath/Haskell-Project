module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 800 -- actual window demension
height = 600 -- actual window demension
offset = 100 -- where does the window populate onto the computer screen

window :: Display -- window properties
window = InWindow "Bouncing Betty" (width, height) (offset, offset)

background :: Color -- background properties
background = black

drawing :: Picture -- create the images for the game
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

-- current game value
-- use game to type check the values passed into PongGame
data PongGame = Game {
  ballLocation :: (Float,Float),
  ballVelocity :: (Float, Float),
  player :: Float
} deriving Show -- so we can read from this structure

-- initial game values
initState :: PongGame
initState = Game {
  ballLocation = (0,0),
  ballVelocity = (-1, -1),
  player = 40
}

-- take the PongGame and create a picture(static image) with it
-- this function is to be used constantly to refresh the current game, ball, and player's pad
-- ??? why do we take player next_game instead of next_game player ???
render :: PongGame -> Picture
render next_game = pictures [ball, walls, mkPaddle green 280 (player next_game) 90]
  where
    -- ball props
    ball = uncurry translate (ballLocation next_game) $ color ballColor $ circleSolid 10
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

moveBall :: Float -> PongGame -> PongGame
position

-- execute our where before we return the new ball location coords
moveBall timePassed current_game = current_game { ballLocation = (x',y') }
  where
    (x, y) = ballLocation current_game
    (xVol, yVol) = ballVelocity current_game
    x' = x + xVol * timePassed
    y' = y + yVol * timePassed




animate :: Display -> Color -> (Float -> Picture) -> IO()
animate = 


main :: IO ()
main = display window background drawing 