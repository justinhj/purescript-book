module Example.Path where

import Prelude

import Data.Array (range, snoc, uncons)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Graphics.Canvas (Context2D, beginPath, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle)
import Partial.Unsafe (unsafePartial)

type Point = { x :: Number, y :: Number }

translate :: forall r . Number -> Number -> Number 
  -> { x :: Number, y :: Number | r } 
  -> { x :: Number, y :: Number | r }
translate dx dy scale shape = shape
  { x = shape.x * scale + dx , 
    y = shape.y * -scale + dy
  }

renderFunction :: Context2D -> (Number -> Point) -> Effect Unit
renderFunction ctx f = 
  let
    pts = map ((\n -> f $ (toNumber n) / 10.0) >>> (translate 300.0 300.0 200.0)) $ range 0 10
 in
   renderPath ctx (snoc pts (translate 300.0 300.0 200.0 {x:1.0, y:0.0}))

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx pts = 
  case uncons pts of 
      Just {head: first, tail: tail} ->
        fillPath ctx $ do
          moveTo ctx first.x first.y
          _ <- traverse (\pt -> lineTo ctx pt.x pt.y) tail
          closePath ctx
      Nothing -> pure unit

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"
  renderFunction ctx (\n -> {x: n, y: n*n*0.33})
