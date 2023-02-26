module Test.MySolutions where

import Prelude

import Control.Bind (composeKleisli)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, modify)
import Data.Foldable (traverse_)
import Data.Monoid (power)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)

testParens :: String -> Boolean
testParens arr = execState (countB $ toCharArray arr) 0 == 0
  where 
    countB :: Array Char -> State Int Unit
    countB = traverse_ (\next -> modify \c -> updateC c next)
      where 
        updateC count next = 
          if next == '(' && count >= 0 then
            count + 1
          else if next == ')' && count >= 0 then
            count - 1
          else
            count


type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line s = do
  ind <- ask
  pure $ (power "  " ind) <> s


indent :: Doc -> Doc
indent = local $ (+) 1

-- sequence converts (Array Reader) to (Reader Array) 
-- x is the array of strings and must return a reader
cat :: Array Doc -> Doc
cat = composeKleisli (traverse (\doc -> doc)) (\x -> pure (joinWith "\n" x))
-- cat = composeKleisli sequence (\x -> pure (joinWith "\n" x))

render :: Doc -> String
render doc = runReader doc 0



