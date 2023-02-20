module Test.MySolutions where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Function.Uncurried (Fn3)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Test.Examples (Complex, Quadratic, Undefined)

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots q = quadraticRootsImpl (\a b -> (Pair a b)) q

foreign import toMaybeImpl :: forall a. (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Undefined a -> Maybe a

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe = toMaybeImpl Just Nothing

foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapJson >>> decodeJson

valuesOfMapGeneric :: forall k v. Ord k => Ord v =>
  EncodeJson k => EncodeJson v => DecodeJson v => Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapJson >>> decodeJson

foreign import quadraticRootsSetImpl :: Json -> Json

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = encodeJson >>> quadraticRootsSetImpl >>> decodeJson
