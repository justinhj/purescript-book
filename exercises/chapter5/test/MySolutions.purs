module Test.MySolutions where

-- 1
-- 1 * 2 = 2
-- 1 * 2 * 3 

import Data.Boolean (otherwise)
import Prelude ((*), (-), (<), (/), (>=))

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

