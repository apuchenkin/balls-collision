module Model where

import Utility
import Linear.V2                      (V2 (..))
import Graphics.Gloss.Data.Color      (Color (..), makeColorI)
import Graphics.Gloss.Data.ViewPort   (ViewPort (..))
import Linear.Vector      ((^-^), (^*), (*^))
import Linear.Matrix      ((!*!))
import Data.Maybe         (catMaybes)
import Control.Lens.Lens  ((<&>))
import Control.Arrow      (first)
import System.Random      (randomRIO)

type Velocity = V2 Float
type Position = V2 Float

data Ball = Ball {
    ballColor :: Color,     -- color
    v         :: Velocity,  -- velocity
    r         :: Float,     -- radius
    position  :: Position   -- position
  }

type Model = [Ball]

gravity :: Float
gravity = 10

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

step :: ViewPort -> Float -> Model -> IO Model
step _ dt balls = return $ fst (foldl stepBalls ([], balls) balls)
  where
    stepBalls :: (Model, Model) -> Ball -> (Model, Model)
    stepBalls (balls, b:balls') _ = do
      let b'   = stepBall b
      let bmap = balls' <&> \b2 -> if checkBallCollision b' b2
            then first Just $ resolveBallCollision b' b2 dt
            else (Nothing, b2)

      let b'' = case catMaybes $ fst <$> bmap of {[] -> b'; x:xs -> x}

      (balls ++ [b''], snd <$> bmap)

    stepBall :: Ball -> Ball
    stepBall b = b''
      where
        a   = V2 0 (-gravity * r b)
        dv  = a  ^* dt
        v'  = v b + dv
        p'  = position b + v' ^* dt
        b'  = b {v = v', position = p'}
        b'' = resolveFrameCollision b' dt $ checkFrameCollision b'

checkFrameCollision :: Ball -> V2 Bool
checkFrameCollision b = V2
    (abs x + r b > (fromIntegral (fst frame) / 2))
    (abs y + r b > (fromIntegral (fst frame) / 2))
    where
      V2 x y = position b

resolveFrameCollision :: Ball -> Float -> V2 Bool -> Ball
resolveFrameCollision b dt cmap = b { v = v', position = p' }
    where
      vmap = fmap (\i -> if i then -1 else 1) cmap
      v' = v b * vmap
      p' = position b + v' ^* dt

checkBallCollision :: Ball -> Ball -> Bool
checkBallCollision b1 b2 = dist < (r b1 + r b2)
    where dist = mag (position b1 ^-^ position b2)

resolveBallCollision :: Ball -> Ball -> Float -> (Ball, Ball)
resolveBallCollision b1 b2 dt = (
    b1 {v = v1', position = p1'},
    b2 {v = v2', position = p2'}
  )
  where
    dv  = v b1 ^-^ v b2
    dv' = negate dv
    dp  = position b1 ^-^ position b2
    dp' = negate dp
    v1' = v b1 - (2 * r b2 / (r b1 + r b2)) * (( dv  `vdot` dp ) / mag2 dp ) *^ dp
    v2' = v b2 - (2 * r b1 / (r b1 + r b2)) * (( dv' `vdot` dp') / mag2 dp') *^ dp'
    p1' = position b1 + v1' ^* dt
    p2' = position b2 + v2' ^* dt
