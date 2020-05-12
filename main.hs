-- TODO: Disco / rave mode
-- TODO: Boomer mode
-- TODO: Select ball type
-- TODO: make width, height, offset a float ??

module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 800 -- actual window demension
height = 600 -- actual window demension
offset = 100 -- where does the window populate onto the computer screen

window :: Display -- window properties
window = InWindow "Bouncing Betty" (width, height) (offset, offset)

background :: Color -- background properties
background = black

fps :: Int
fps = 60

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
  ballVelocity = (-50, -50),
  player = 0
}

-- take the PongGame and create a picture(static image) with it
-- this function is to be used constantly to refresh the current game, ball, and player's pad
-- ??? why do we take player current_game instead of current_game player ???
render :: PongGame -> Picture
render current_game = pictures [ball, walls, mkPaddle green 280 (player current_game) 90]
  where
    -- ball props
    ball = uncurry translate (ballLocation current_game) $ color ballColor $ circleSolid 10
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
-- executes where before we return the new ball location coords
moveBall timePassed current_game = current_game { ballLocation = (x',y') }
  where
    (x, y) = ballLocation current_game
    (xVol, yVol) = ballVelocity current_game
    x' = x + xVol * timePassed
    y' = y + yVol * timePassed


main :: IO ()
main = play window background fps initState render handleKeys update

-- update the game
update :: ViewPort -> Float -> PongGame -> PongGame
update _ timePassed = checkWallCollision . moveBall timePassed

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 's') _ _ _) current_game = game { ballLocation = (0,0) }

handleKeys _ current_game = game



-- ~~ collision detection ~~

type Radius = Float
type Position = (Float, Float)

heightCollision :: Position -> Radius -> Bool
heightCollision (_, y) radius = tc
  where
    tc = y + radius >= fromIntegral height

widthCollision :: Position -> Radius -> Bool
widthCollision (x, _) radius = leftCollision || rightCollision
  where
    leftCollision = x - radius >= -fromIntegral width
    rightCollision = x + radius >= fromIntegral width 

checkWallCollision :: PongGame -> PongGame
-- we already know where the walls are placed ... 
-- find out if ball hits the walls and if collision detected so adjust ball trajectory
checkWallCollision current_game = current_game { ballVelocity = (xVol', yVol')}
  where
    radius = 10
    (xVol, yVol) = ballVelocity current_game

    xVol' = if widthCollision (ballLocation current_game) radius
            then
              -xVol
            else
              xVol
    yVol' = if heightCollision (ballLocation current_game) radius
            then
              -yVol
            else
              yVol

paddleCollision :: Position -> Radius -> PongGame -> Bool
paddleCollision (x, y) radius current_game = collision
  where
    radius = 10
    currX = player current_game
    collision = (106 <= y + radius) && (abs (x - currX) < 53)
    -- not sure if abs is correct

paddleBounce :: PongGame -> PongGame
-- take in a current PG and output the new PG
-- find out where the ball is, where the paddle is, adjust ball location upon collision
paddleBounce current_game = current_game {ballVelocity = (xVol, yVol')}
  where
    radius = 10
    (xVol, yVol) = ballVelocity current_game

    yVol' = if paddleCollision (ballLocation current_game) radius current_game
            then
              -yVol
            else
              yVol