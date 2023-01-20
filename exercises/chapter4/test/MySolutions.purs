module Test.MySolutions where

import Prelude (mod, ($), (+), (==))
import Data.Array (head,drop)
import Data.Maybe (Maybe(..))

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
