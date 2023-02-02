module Test.MySolutions where

import ChapterExamples (Amp(..), Volt(..))
import Data.Boolean (otherwise)
import Data.Maybe (Maybe(..))
import Data.Number (abs, pi)
import Data.Person (Person)
import Data.Picture (Shape(..))
import Prelude (negate, (*), (+), (-), (/), (<), (==), (>=))

factorial :: Int -> Int 
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

factorial' :: Int -> Int 
factorial' n 
  | n<2 = 1
  | otherwise = n * factorial' (n - 1)

-- n! / k! (n - k)!
-- What is the formula for choosing K out of n elements in a set?
-- The formula for N choose K is given as: C(n, k)= n!/[k!( n-k)!]
-- 10 0 = 1
-- 0 3 = 0
-- 2 5 = 0
-- 10 5 = 252
-- 5 5 = 1
binomial :: Int -> Int -> Int
binomial 0 _ = 0
binomial _ 0 = 1
binomial n k | n >= k = factorial n / ((factorial k) * (factorial (n - k)))
             | otherwise = 0

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k
  = pascal (n-1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } }  = city1 == city2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton default _ = default

-- Shape stuff

circleAtOrigin :: Shape
circleAtOrigin = Circle { x: 0.0, y: 0.0 } 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle { x:0.0, y:0.0 } (r * 2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle { x: 0.0, y: 0.0 } (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line {x: x1, y: y1} {x: x2, y: y2}) = 
  Line { x: -w / 2.0, y: -h / 2.0 } { x: w / 2.0, y: h / 2.0 } 
  where
    w = 2.0 * abs (x1 - x2)
    h = 2.0 * abs (y1 - y2)
doubleScaleAndCenter (Text _ text) = Text {x:0.0, y:0.0} text
doubleScaleAndCenter (Clipped pic loc w h) = Clipped pic loc w h

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp amps) (Volt volts) = Watt (amps * volts)

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Line _ _) = 0.0
area (Text _ _) = 0.0
area (Clipped _ _ _ _) = 0.0


