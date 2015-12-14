module Model where

import Utility
import Linear.V2                      (V2 (..))
import Graphics.Gloss.Data.Color      (Color (..), makeColorI)
import Graphics.Gloss.Data.ViewPort   (ViewPort (..))
import Linear.Vector                  ((^-^), (^*), (*^))
import Linear.Matrix                  ((!*!))
import Data.Maybe                     (catMaybes, isNothing, fromJust)
import Control.Lens.Lens              ((<&>))
import Control.Arrow                  (first)
import System.Random                  (randomRIO)
import Control.Monad.Loops            (iterateUntilM)
import Data.Sequence                  (Seq, fromList, update, elemIndexL, index)
import Data.Foldable                  (toList)

type Velocity = V2 Float
type Position = V2 Float

data Event = CX Ball Float | CY Ball Float | CB Ball Ball Float

eventTime :: Event -> Float
eventTime e = case e of
  CX _    t -> t
  CY _    t -> t
  CB _ _  t -> t

instance Show Event where
  show e = show $ eventTime e

instance Eq  Event where
  (==) e1 e2 = eventTime e1 == eventTime e2

instance Ord Event where
  compare e1 e2 = eventTime e1 `compare` eventTime e2

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
step _ dt balls = do
  r <- iterateUntilM (isNothing . fst) (\(mdt, balls) -> do
    let dt     = fromJust mdt
    let sballs = fromList balls
    let emap   = getWallCollision dt <$> sballs
    let emin   = case catMaybes $ toList emap of {[] -> Nothing; a -> Just $ minimum a}
    let balls' = case emin of
          Nothing -> stepBall dt <$> balls
          Just  e -> do
            let idx = fromJust $ elemIndexL emin emap
            let t   = eventTime e
            let sballs' = stepBall t <$> sballs
            let sballs'' = update idx (resolveEvent e) sballs'
            toList sballs''

    let dt' = emin <&> ((\t -> dt - t) . eventTime)
    print (dt, dt')
    return (dt' , balls')) (Just dt, balls)
  return $ snd r
  where
    stepBall :: Float -> Ball -> Ball
    stepBall dt b = b'
      where
        a   = V2 0 (-gravity * r b)
        dv  = a  ^* dt
        v'  = v b + dv
        p'  = position b + v' ^* dt
        b'  = b {v = v', position = p'}

resolveEvent :: Event -> Ball
resolveEvent e = case e of
  CX b t      -> b { v = v b * V2 (-1) 1}
  CY b t      -> b { v = v b * V2 1 (-1)}
  CB b1 b2 t  -> error "not supported"

  -- resolveEvent e = e

  -- $ fst (foldl stepBalls ([], balls) balls)
  -- where
  --   stepBalls :: (Model, Model) -> Ball -> (Model, Model)
  --   stepBalls (balls, b:balls') _ = do
  --     let b'   = stepBall b
  --     let bmap = balls' <&> \b2 -> if checkBallCollision b' b2
  --           then first Just $ resolveBallCollision b' b2 dt
  --           else (Nothing, b2)
  --
  --     let b'' = case catMaybes $ fst <$> bmap of {[] -> b'; x:xs -> x}
  --
  --     (balls ++ [b''], snd <$> bmap)
  --



getWallCollision :: Float -> Ball -> Maybe Event
getWallCollision dt b = if tx <= ty then tx else ty
    where
      V2 vx vy = v b
      V2 x y   = position b
      dx       = (fromIntegral (fst frame) / 2)  - (abs x + r b)
      dy       = (fromIntegral (snd frame) / 2)  - (abs y + r b)
      ty  | vy == 0             = Nothing
          | (abs dy / abs vy) >= dt   = Nothing
          | (abs dy / abs vy) < dt    = Just $ CY b (abs dy / abs vy)
      tx  | vx == 0             = Nothing
          | (abs dx / abs vx) >= dt   = Nothing
          | (abs dx / abs vx) < dt    = Just $ CX b (abs dx / abs vx)


getBallCollistion :: Ball -> Ball -> Float -> Maybe Float
getBallCollistion b1 b2 dt = Nothing

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
