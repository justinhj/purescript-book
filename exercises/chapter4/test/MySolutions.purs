module Test.MySolutions where

import Data.Path
import Control.Alternative (guard)
import Control.Bind (bind, discard, pure, join)
import Data.Array (cons, drop, filter, foldl, head, tail, index, length, (..), (:), find, last, reverse, sortWith)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), split)
import Debug (spy)
import Prelude (div, map, mod, ($), (&&), (*), (+), (-), (<), (==), (>=), not, (>))
import Data.Tuple (snd,Tuple(..))
import Control.Semigroupoid ((<<<))

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
      Nothing -> acc
    
primeFactors :: Int -> Array Int
primeFactors n = reverse $ helper c 0 n []
  where c = candidates n

allTrue :: Array Boolean -> Boolean
allTrue xs = foldl (\x1 x2 -> x1 && x2) true xs

fibWithSpy :: Int -> Int
fibWithSpy n =
  if n == 0 then
    0
  else if n == 1 then
    1
  else
    fibWithSpy ((spy "n-1" n) - 1) + fibWithSpy ((spy "n-2") n - 2)

fibTailRecWithSpy :: Int -> Int
fibTailRecWithSpy 0 = 0
fibTailRecWithSpy 1 = 1
fibTailRecWithSpy n = fibH 1 0 (n - 2) 
  where
    fibH :: Int -> Int -> Int -> Int
    fibH a b i = 
      if i == 0 then 
        a + b
      else
        fibH (a + b) a ((spy "i" i) - 1)

-- Same as above with spy removed
fib :: Int -> Int
fib n =
  if n == 0 then
    0
  else if n == 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fibH 1 0 (n - 2) 
  where
    fibH :: Int -> Int -> Int -> Int
    fibH a b i = 
      if i == 0 then 
        a + b
      else
        fibH (a + b) a (i - 1)

-- reverse :: ∀ a. Array a -> Array a
-- reverse xs = foldl (\acc n -> cons n acc) [] xs

allFiles :: Path -> Array Path
allFiles file = file : do
  child <- ls file
  allFiles child

allMatches :: Path -> Path -> String -> Array (Tuple Path Path)
allMatches path parent name = 
  if fileName' path == name then
    cons (Tuple path parent) rest
  else
    rest
  where rest = do
                 element <- ls path
                 allMatches element path name

allMatch' :: Path -> Path -> String -> Array (Tuple Path Path)
allMatch' path parent name = cons (Tuple path parent) do
  element <- ls path
  allMatch' element path name

allMatch :: Path -> String -> Array Path
allMatch p n = filter (\x -> fileName' x == n) (allFiles p)

whereIs :: Path -> String -> Maybe Path
whereIs p n = map snd (last $ allMatches p p n)

onlyFiles :: Path -> Array Path
onlyFiles p = filter (not isDirectory) files
  where files = allFiles p

fileName' :: Path -> String
fileName' (File name _) = 
  case lp of
      Just n -> n
      Nothing -> "" -- should be an error?
    where lp = last $ split (Pattern "/") name
fileName' (Directory name _) = name

-- maxFile :: Array Path -> Int -> Maybe Path -> Maybe Path
-- maxFile paths maxSize maxFile =
--       case head paths of
--         first@(Just (File _ size)) ->
--           if size > maxSize then
--             case tail paths of
--               Just [] -> head
--               Just rest -> (maxFile rest size first)
--           else
--             case tail paths of
--               [] -> maxFile
--               rest -> maxFile rest maxSize maxFile


largestSmallest :: Path -> Array Path
largestSmallest file@(File _ _) = [file]
largestSmallest path = 
  let
      off = filter (not <<< isDirectory) (allFiles path)
      min = sortWith size off
      max = reverse min
      hmin = head min
      hmax = head max
    in
      case [hmin,hmax] of
          [Just a, Just b] -> 
            if filename a == filename b then
              [a]
            else
              [a,b]
          [Nothing, Just a] -> [a]
          [Just a, Nothing] -> [a]
          _ -> []

