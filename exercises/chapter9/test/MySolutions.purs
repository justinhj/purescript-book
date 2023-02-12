module Test.MySolutions where

import Prelude

import Data.Either (Either)
import Data.Foldable (fold)
import Data.String.CodePoints (length)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, attempt)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, readTextFile)
import Node.Path (FilePath)

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles in1 in2 out = do
  data1 <- readTextFile UTF8 in1
  data2 <- readTextFile UTF8 in2
  appendTextFile UTF8 out data1
  appendTextFile UTF8 out data2

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany arr out = do 
  contents <- traverse (\f -> readTextFile UTF8 f) arr
  appendTextFile UTF8 out $ fold contents

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters f = attempt do
  contents <- readTextFile UTF8 f
  pure $ length contents
