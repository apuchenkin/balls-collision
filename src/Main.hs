module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Linear.V2          (V2 (..))
import Linear.Vector      ((^-^), (^*), (*^))
import Linear.Matrix      ((!*!))
import Data.Maybe         (catMaybes)
import Control.Lens.Lens  ((<&>))
import Control.Arrow      (first)
import System.Random      (randomRIO)
import Control.Monad      (replicateM)

data Ball = Ball {
    ballColor :: Color,
    v         :: V2 Float,
    r         :: Float,
    position  :: V2 Float
  }

type Model = [Ball]

frame :: (Int, Int)
frame = (600, 600)

createBall :: IO Ball
createBall = do
  let x0 = fst frame `div` 2
  let y0 = snd frame `div` 2
  vx <- randomRIO (-50, 50)
  vy <- randomRIO (-50, 50)
  br <- randomRIO (5, 30)
  x  <- randomRIO (br - x0, fst frame - br - x0)
  y  <- randomRIO (br - y0, snd frame - br - y0)

  cr <- randomRIO (0, 255)
  cg <- randomRIO (0, 255)
  cb <- randomRIO (0, 255)

  return Ball {
      ballColor   = makeColorI cr cg cb 255,
      v           = V2 vx vy,
      r           = fromIntegral br,
      position    = V2 (fromIntegral x) (fromIntegral y)
    }

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

step :: ViewPort -> Float -> Model -> IO Model
step vp t m = return $ fst (foldl stepBalls ([],m) m)
  where
    stepBalls :: (Model, Model) -> Ball -> (Model, Model)
    stepBalls (balls, b:balls') _ = do
      let b'   = stepBall b
      let bmap = balls' <&> \b2 -> if checkBallCollision b' b2
            then first Just $ resolveBallCollision b' b2
            else (Nothing, b2)

      let b'' = case catMaybes $ fst <$> bmap of {[] -> b'; x:xs -> x}

      (balls ++ [b''], snd <$> bmap)

    stepBall :: Ball -> Ball
    stepBall b = b {v = v', position = p''}
      where
        p'  = position b + (v b ^* t)
        v'  = resolveFrameCollision b $ checkFrameCollision b p'
        p'' = position b + (v' ^* t)

checkFrameCollision :: Ball -> V2 Float -> V2 Bool
checkFrameCollision b (V2 x y) = V2
    (abs x + r b > (fromIntegral (fst frame) / 2))
    (abs y + r b > (fromIntegral (fst frame) / 2))

resolveFrameCollision :: Ball -> V2 Bool -> V2 Float
resolveFrameCollision b m = v b * m'
    where m' = fmap (\i -> if i then -1 else 1) m

checkBallCollision :: Ball -> Ball -> Bool
checkBallCollision b1 b2 = dist < (r b1 + r b2)
    where dist = mag (position b1 ^-^ position b2)

resolveBallCollision :: Ball -> Ball -> (Ball, Ball)
resolveBallCollision b1 b2 = (
    b1 {v = v b1 - (2 * r b2 / (r b1 + r b2)) * (( dv  `vdot` dp ) / mag2 dp ) *^ dp },
    b2 {v = v b2 - (2 * r b1 / (r b1 + r b2)) * (( dv' `vdot` dp') / mag2 dp') *^ dp'}
  )
  where
    dv  = v b1 ^-^ v b2
    dv' = v b2 ^-^ v b1
    dp  = position b1 ^-^ position b2
    dp' = position b2 ^-^ position b1

vdot :: V2 Float -> V2 Float -> Float
vdot (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

mag :: V2 Float -> Float
mag = sqrt . mag2

mag2 :: V2 Float -> Float
mag2 v = vdot v v

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
