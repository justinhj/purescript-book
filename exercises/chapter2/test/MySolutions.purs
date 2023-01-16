module Test.MySolutions where

import Prelude
import Data.Number (sqrt,pi)
import Data.Int (rem)

diagonal :: Number -> Number -> Number
diagonal b h = sqrt ((b * b) + (h * h))

circleArea :: Number -> Number
circleArea r = pi * r * r 

leftoverCents :: Int -> Int
leftoverCents a = rem a 100
