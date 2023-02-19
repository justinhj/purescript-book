module Test.MySolutions where

import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair(..))
import Test.Examples (Complex, Quadratic)

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots q = quadraticRootsImpl (\a b -> (Pair a b)) q
