module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe l r = lift2 (+) l r

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe l r = lift2 (-) l r

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe l r = lift2 (*) l r

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe l r = lift2 (/) l r

addApply :: forall f. Apply f => f Int -> f Int -> f Int
addApply l r = lift2 (+) l r

subApply :: forall f. Apply f => f Int -> f Int -> f Int
subApply l r = lift2 (-) l r

mulApply :: forall f. Apply f => f Int -> f Int -> f Int
mulApply l r = lift2 (*) l r

divApply :: forall f. Apply f => f Int -> f Int -> f Int
divApply l r = lift2 (/) l r
