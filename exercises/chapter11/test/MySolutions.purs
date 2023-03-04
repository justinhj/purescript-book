module Test.MySolutions where

import Prelude

import Control.Bind (composeKleisli)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, ReaderT, ask, local, runReader, runReaderT)
import Control.Monad.State (State, StateT, execState, get, modify, put)
import Control.Monad.Writer (Writer, WriterT, execWriterT, lift, runWriter, tell)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, joinWith, stripPrefix, take)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
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
collatz n = case runWriter $ collatz' n of
  (Tuple _ w) -> Tuple ((length w) - 1) w

collatz' :: Int -> Writer (Array Int) Int
collatz' n | n == 1 = do
            tell [1]
            pure 1
            | n `mod` 2 == 1 = do
                tell [n]
                collatz' newval
              where newval = 3 * n + 1
            | otherwise = do
                tell [n]
                collatz' newval
              where newval = n / 2

safeDivide :: Int -> Int -> ExceptT String Identity Int
safeDivide a b = do
  if b == 0 then
    throwError "Division by zero :(" 
  else
    pure $ a / b
   
type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

-- stripPrefix
string :: String -> Parser String
string prefix = do
  st <- get
  tell ["The state is " <> st]
  case stripPrefix (Pattern prefix) st of
      Just rest -> do
         put rest
         pure prefix
      Nothing -> throwError ["Could not parse"]

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " <> s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)


-- Use the ReaderT and WriterT monad transformers to reimplement the document printing
-- library which we wrote earlier using the Reader monad.

-- Instead of using line to emit strings and cat to concatenate strings, use the Array
-- String monoid with the WriterT monad transformer, and tell to append a line to the
-- result. Use the same names as in the original implementation but ending with an 
-- apostrophe (').

type Level' = Int
type Doc' = WriterT (Array String) (ReaderT Level' (ExceptT String Identity)) Unit
-- type Doc' = WriterT (Array String) (ReaderT Level' Identity) Unit

line' :: String -> Doc'
line' s = do
  ind <- lift $ ask
  tell $ [(power "  " ind) <> s]
  pure unit

-- indent' = local $ (+) 1

-- Return an error should indent be over 3

indent' :: Doc' -> Doc'
indent' doc = do
  ind <- ask
  if ind < 3 then
    (local $ (+) 1) doc
  else
    throwError "Too much indent"

-- This does a bit more than the exercise in the book. I've added ExceptT to the 
-- Doc' type, the render function now has to unwrap the response and handle the 
-- error case. Errors are thrown if the indent reaches 3.
render' :: Doc' -> String
render' doct = 
  let 
      out = unwrap $ runExceptT $ runReaderT (execWriterT $ doct) 0
  in
    case out of 
        Right arr -> joinWith "\n" arr
        Left err -> "Error! " <> err


