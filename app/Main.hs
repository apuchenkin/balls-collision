module Main where

import Model
import Graphics.Gloss                         (Display (..))
import Graphics.Gloss.Data.Color              (black)
import Graphics.Gloss.Interface.IO.Simulate   (simulateIO)
import Control.Monad                          (replicateM)

main :: IO ()
main =  do
  model <- replicateM 30 createBall
  simulateIO
    (InWindow "Balls" frame (50,  50))
    black
    60
    model
    drawModel
    step
