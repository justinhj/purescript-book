module Test.MySolutions where

import Prelude

import Control.Applicative (pure, apply)
import Control.Apply (apply, lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, lengthIs, matches, nonEmpty)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Data.Validation.Semigroup (V)

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

combineMaybe' :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe' fa = sequence fa

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = map Just x 
combineMaybe _ = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z][a-zA-Z]$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S+" noFlags

validateAddressImproved' :: Address -> V Errors Address
validateAddressImproved' a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state

-- As above but with applicative do syntax
validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- matches "Street" nonEmptyRegex a.street
  city <- matches "City" nonEmptyRegex a.city
  state <- matches "State" stateRegex a.state
in address street city state


data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- (Branch (Branch Leaf 8 Leaf) 42 Leaf)
instance showTree :: (Show a) => Show (Tree a) where
  show (Branch l a r) = "(Branch " <> (show l) <> " " <> (show a) <> " " <> (show r) <> ")"
  show Leaf = "Leaf"

derive instance eqTree :: Eq a => Eq (Tree a)
