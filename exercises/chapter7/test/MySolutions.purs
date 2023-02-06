module Test.MySolutions where

import Prelude

import Control.Applicative (pure, apply)
import Control.Apply (apply, lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, lengthIs, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V, invalid)

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

derive instance treeFunctor :: Functor Tree
derive instance treeFoldable :: Foldable Tree

-- traverse :: forall a b m. Applicative m => (a -> m b) -> List a -> m (List b) 
instance traverseTree :: Traversable Tree where
  traverse f (Branch l a r) = Branch <$> (traverse f l) <*> (f a) <*> (traverse f r)
  traverse _ _ = pure Leaf
  sequence = traverse identity

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder f (Branch t1 v t2) = ado
  mv <- f v
  mt1 <- traversePreOrder f t1
  mt2 <- traversePreOrder f t2
  in Branch mt1 mv mt2
traversePreOrder _ Leaf = pure Leaf

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder f (Branch t1 v t2) = ado
  mt1 <- traversePostOrder f t1
  mt2 <- traversePostOrder f t2
  mv <- f v
  in Branch mt1 mv mt2
traversePostOrder _ Leaf = pure Leaf


type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p = personOptionalAddress
    <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse thing = traverse identity thing

traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f thing = sequence (map f thing)
