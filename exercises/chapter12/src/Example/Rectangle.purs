module Example.Rectangle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, setStrokeStyle)
import Partial.Unsafe (unsafePartial)

-- ANCHOR: main
main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
-- ANCHOR_END: main

-- ANCHOR: setFillStyle
  setFillStyle ctx "#00F"
-- ANCHOR_END: setFillStyle

-- ANCHOR: fillPath
  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }
-- ANCHOR_END: fillPath

  setFillStyle ctx "rgba(0, 0, 255, 0.5)"

  fillPath ctx $ do
    moveTo ctx 250.0 250.0
    lineTo ctx 370.0 250.0
    lineTo ctx 370.0 370.0
    lineTo ctx 250.0 370.0
    lineTo ctx 250.0 250.0
    closePath ctx
