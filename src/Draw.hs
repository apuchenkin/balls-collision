module Draw where

import Model
import Linear.V2                   (V2 (..))
import Graphics.Gloss.Data.Picture (Picture (..), translate, color, circleSolid, polygon, rectanglePath)
import Graphics.Gloss.Data.Color   (white)

drawBall :: Ball -> IO Picture
drawBall b = return $ color (ballColor b) $ translate x y circle
  where
    V2 x y = position b
    circle = circleSolid $ r b

drawModel :: Model -> IO Picture
drawModel m = do
  pictures <- sequence $ initPath : (drawBall <$> m)
  return $ Pictures pictures
  where
    initPath :: IO Picture
    initPath = return $ color white $ polygon $ rectanglePath
      (fromIntegral (fst frame))
      (fromIntegral (snd frame))
