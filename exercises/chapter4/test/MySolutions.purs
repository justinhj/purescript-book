module Test.MySolutions where

import Control.Alternative (guard)
import Control.Bind (bind, discard, pure)
import Data.Array (cons, drop, filter, head, index, length, reverse, (..))
import Data.Maybe (Maybe(..))
import Prelude (div, map, mod, ($), (&&), (*), (+), (<), (==), (>=))

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEven :: Array Int -> Int
countEven elements = 
  case head elements of
    Just n -> 
      if isEven n then
        1 + next
      else
        next
        where 
          next = (countEven $ drop 1 elements)
    Nothing -> 0

squared :: Array Number -> Array Number
squared ns = map (\x -> x * x) ns

keepNonNegative :: Array Number -> Array Number
keepNonNegative ns = filter (\x -> x >= 0.0) ns

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite ns = (\x -> x >= 0.0) <$?> ns

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- 1 .. n 
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 2

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  a1 <- a
  b1 <- b
  pure [a1, b1]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ (a*a) + (b*b) == (c*c) && a < b
  pure [a,b,c]

-- Get ascending primes over 1 and less than n
candidates :: Int -> Array Int
candidates n = if n < 2 then
    []
  else
    filter isPrime (2 .. n)

helper :: Array Int -> Int -> Int -> Array Int -> Array Int
helper primes i remain acc =
  case index primes i of
      Just n -> 
        if remain `mod` n == 0 then
          helper primes i (remain `div` n) (cons n acc)
        else
          helper primes (i + 1) remain acc 
      Nothing -> reverse acc
    
primeFactors :: Int -> Array Int
primeFactors n = helper c 0 n []
  where c = candidates n
