module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Math (pi, sqrt)

circleArea :: Number -> Number
circleArea r = r * r * pi

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main :: Effect Unit
-- main = logShow (diagonal 3.0 4.0)
main = logShow (circleArea 3.0)
