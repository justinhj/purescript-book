module Test.MySolutions where

import Prelude

import Control.Monad.State (State, execState, modify)
import Data.Foldable (traverse_)
import Data.String.CodeUnits (toCharArray)

testParens :: String -> Boolean
testParens arr = execState (countB $ toCharArray arr) 0 == 0

countB :: Array Char -> State Int Unit
countB = traverse_ (\next -> modify \c -> updateC c next)
  where 
    updateC count next = 
      if count < 0 then
        count
      else if next == '(' then
        count + 1
      else if next == ')' then
        count - 1
      else
        count
