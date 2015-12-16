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
import Control.Monad                  (join)
import Control.Monad.Loops            (iterateUntilM)
import Data.Sequence                  (Seq, fromList, update, elemIndexL, elemIndicesL, index, mapWithIndex, (><))
import Data.Foldable                  (toList)
import qualified Data.Sequence as S   (drop)

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
  } deriving Show

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
  br <- randomRIO (5, 20)
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
  r <- iterateUntilM (isNothing . fst) stepBalls (Just dt, fromList balls)
  return $ toList (snd r)
  where
    listMin :: Ord a => [Maybe a] -> Maybe a
    listMin list = case catMaybes list of {[] -> Nothing; a -> Just $ minimum a}

    stepBall :: Float -> Ball -> Ball
    stepBall dt b = b'
      where
        a   = V2 0 (-gravity * r b)
        dv  = a ^* dt
        v'  = v b + dv
        p'  = position b + v' ^* dt
        b'  = b {v = v', position = p'}

    stepBalls :: (Maybe Float, Seq Ball) -> IO (Maybe Float, Seq Ball)
    stepBalls (mdt, balls) = return (mdt' , balls')
      where
        dt     = fromJust mdt
        emap   = getWallCollision dt <$> balls
        emap'  = flip mapWithIndex balls $ \i b -> do
            let s' = S.drop i balls
            let events = getBallCollision dt b <$> balls
            listMin $ toList events

        emin   = listMin $ toList (emap >< emap')
        mdt'   = emin <&> ((\t -> dt - t) . eventTime)
        balls' = case emin of
            Nothing -> stepBall dt <$> balls
            Just  e -> do
              let events = case e of {CB {} -> emap'; _ -> emap}
              let idx = elemIndicesL emin events
              let t   = eventTime e
              let balls' = stepBall t <$> balls
              foldl (\r i -> update i (resolveEvent $ fromJust $ index events i) r) balls' idx

resolveEvent :: Event -> Ball
resolveEvent e = case e of
  CX b t      -> b { v = v b * V2 (-1) 1}
  CY b t      -> b { v = v b * V2 1 (-1)}
  CB b1 b2 t  -> fst $ resolveBallCollision b1 b2

getWallCollision :: Float -> Ball -> Maybe Event
getWallCollision dt b = e'
    where
      V2 vx vy = v b
      V2 x y   = position b
      mdx      = fromIntegral (fst frame) / 2
      mdy      = fromIntegral (snd frame) / 2
      dx       = abs $ mdx + (if vx > 0 then -x else x) - r b
      dy       = abs $ mdy + (if vy > 0 then -y else y) - r b
      ex       = CX b $ dx / abs vx
      ey       = CY b $ dy / abs vy
      e        = minimum [ex,ey]
      t        = eventTime e
      e' | isInfinite t       = Nothing
         | t > dt             = Nothing
         | otherwise          = Just e

getBallCollision :: Float -> Ball -> Ball -> Maybe Event
getBallCollision dt b1 b2 = e
  where
    dr  = position b2 ^-^ position b1
    dv  = v b2 ^-^ v b1
    dvr = dv `vdot` dr
    rr  = (r b1 + r b2) ^ 2
    d   = (dvr ^ 2) - mag2 dv * (mag2 dr - rr)

    t  | dvr >= 0  = read "Infinity"
       | d   <  0  = read "Infinity"
       | otherwise = - (dvr + sqrt d) / mag2 dv

    e  | isInfinite t = Nothing
       | t  > dt      = Nothing
       | t  <= 0      = Nothing
       | otherwise    = Just $ CB b1 b2 t


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

resolveBallCollision :: Ball -> Ball -> (Ball, Ball)
resolveBallCollision b1 b2 = (b1 {v = v1'}, b2 {v = v2'})
  where
    dv  = v b1 ^-^ v b2
    dv' = negate dv
    dp  = position b1 ^-^ position b2
    dp' = negate dp
    v1' = v b1 - (2 * r b2 / (r b1 + r b2)) * (( dv  `vdot` dp ) / mag2 dp ) *^ dp
    v2' = v b2 - (2 * r b1 / (r b1 + r b2)) * (( dv' `vdot` dp') / mag2 dp') *^ dp'
