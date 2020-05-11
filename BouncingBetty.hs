module Main where

import GameConstants
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import System.IO

-- Main method for the entire game
main :: IO () 
main = do
  -- constantPics <- mkGalagaPics
  playIO window black 60 ()

window :: Display
window = InWindow "Bouncing Betty" screenSize (10,10)

