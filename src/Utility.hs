module Utility where

import Linear.V2 (V2 (..))

vdot :: V2 Float -> V2 Float -> Float
vdot (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

mag :: V2 Float -> Float
mag = sqrt . mag2

mag2 :: V2 Float -> Float
mag2 v = vdot v v
