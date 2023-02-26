module Test.MySolutions where

import Data.Monoid.Additive
import Prelude

import Control.Bind (composeKleisli)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (cons, length, reverse)
import Data.Foldable (traverse_)
import Data.Monoid (power)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

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

cat :: Array Doc -> Doc
cat = composeKleisli (traverse (\doc -> doc)) (\x -> pure (joinWith "\n" x))

render :: Doc -> String
render doc = runReader doc 0

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> do
  tell $ Additive n
  pure unit

collatz :: Int -> Tuple Int (Array Int)
collatz n = report $ runWriter $ collatz'' n 
  where 
    report (Tuple _ w) = Tuple ((length w) - 1) w

-- This works but isn't what was asked for
-- collatz' :: Tuple Int (Array Int) -> Tuple Int (Array Int)
-- collatz' (Tuple n arr) | n == 1 = Tuple ((length arr) - 1) (reverse arr)
--                        | n `mod` 2 == 1 = collatz' (Tuple newval (cons newval arr))
--                           where newval = 3 * n + 1 
--                        | otherwise = collatz' (Tuple newval (cons newval arr))
--                           where newval = n / 2

collatz'' :: Int -> Writer (Array Int) Int
collatz'' n | n == 1 = do
            tell [1]
            pure 1
            | n `mod` 2 == 1 = do
                tell [n]
                collatz'' newval
              where newval = 3 * n + 1
            | otherwise = do
                tell [n]
                collatz'' newval
              where newval = n / 2


