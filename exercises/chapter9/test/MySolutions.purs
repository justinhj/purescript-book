module Test.MySolutions where

import Prelude

import Affjax.Node as AN
import Affjax.ResponseFormat as AXRF
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parOneOf, parTraverse)
import Data.Either (Either(..), hush)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodePoints (length)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path
import Data.Array (concat, (:))

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

writeGet :: String -> FilePath -> Aff Unit
writeGet url out = do
  result <- AN.get ResponseFormat.string url
  case result of
    Left _ -> pure unit
    Right response -> writeTextFile UTF8 out response.body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel arr out = do 
  contents <- parTraverse (\f -> readTextFile UTF8 f) arr
  appendTextFile UTF8 out $ fold contents

getMaybeUrl :: String -> Aff (Maybe String)
getMaybeUrl url = do
  fetch <- AN.get ResponseFormat.string url
  case fetch of
      Left _ -> pure Nothing
      Right response -> pure $ Just response.body

-- <#> is mapFlipped
-- hush turns an Either into a Maybe
getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout time url = do
  parOneOf
    [ AN.get AXRF.string url <#> hush <#> map _.body
    , delay (Milliseconds time) $> Nothing
    ]

fileLines :: FilePath -> Aff (Maybe (Array String))
fileLines f = do
  contents <- readTextFile UTF8 f 
  case contents of
      "" -> pure Nothing
      c -> 
        pure $ Just $ split (Pattern "\n") c

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles file = do
  contents <- readTextFile UTF8 file
  case contents of
    "" -> pure [ file ]
    c -> do
      let
        dir = Path.dirname file
        files = split (Pattern "\n") c
        filesFromRoot = map (\f -> Path.concat [ dir, f ]) files
      arrarr <- parTraverse recurseFiles filesFromRoot
      pure $ file : concat arrarr


