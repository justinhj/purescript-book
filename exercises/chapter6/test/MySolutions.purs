module Test.MySolutions where

import Prelude

import Data.Array (cons, length, nub, nubByEq, nubEq)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Monoid (power)
import Data.Newtype (class Newtype, wrap, over2)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray)

newtype Point
      = Point
      { x :: Number
      , y :: Number
      }

instance showPoint :: Show Point where
  show (Point { x: x, y: y }) =
    "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real: r, imaginary: i }) =
    show r <> sign <> show i <> "i"
    where sign = if i < 0.0 then "" else "+"

derive instance eqComplex :: Eq Complex

derive instance ntComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  zero = wrap { real : 0.0, imaginary : 0.0 }
  one = wrap { real : 1.0, imaginary : 0.0 }
  add = over2 Complex add
  mul (Complex {real:r1, imaginary:i1}) (Complex {real:r2, imaginary:i2}) =
    wrap { real: real, imaginary: imaginary }
    where real = r1 * r2 - i1 * i2
          imaginary = r1 * i2 + i1 * r2

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a b) (NonEmpty c d) = NonEmpty a (b <> (cons c d))  

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a b) (NonEmpty c d) = a == c && b == d

derive instance functorNonEmpty :: Functor NonEmpty

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare Infinite Infinite = EQ
  compare (Finite a) (Finite b) = compare a b

instance nonemptyFoldable :: Foldable NonEmpty where
  foldl f z (NonEmpty a b) = foldl f z (cons a b)
  foldr f z (NonEmpty a b) = foldr f z (cons a b)
  foldMap f (NonEmpty a b) = foldMap f (cons a b)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
   foldr ff z (OneMore a fa) = ff a lastz 
    where lastz = foldr ff z fa
   foldl ff z (OneMore a fa) = foldl ff firstz fa
    where firstz = ff z a 
   foldMap ff (OneMore a fa) = (ff a) <> (foldMap ff fa)

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = fromJust $ maximum arr

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance multShow :: Show Multiply
derive newtype instance eqShow :: Eq Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply m) n = m * n

instance actionMultiplyString :: Action Multiply String where
  act (Multiply s) n = power n s

instance actionArray :: Action m a => Action m (Array a) where
  act m arr = map (act m) arr 

newtype Self m = Self m

-- derive newtype instance eqSelf :: Eq Self _
derive newtype instance showSelf :: Show m => Show (Self m)
derive newtype instance eqSelf :: Eq m => Eq (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

newtype HashCode = HashCode Int

instance hashCodeEq :: Eq HashCode where
  eq (HashCode a) (HashCode b) = a == b

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

derive newtype instance hashcodeShow :: Show HashCode

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates arr = not ((length arr) == (length arr2))
                         && not ((length arr) == (length arr3))
  where
    arr2 = nubByEq hashEqual arr
    arr3 = nubEq arr

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

-- instance hashHour :: Hashable Hour where
--   hash (Hour n) = hashCode n

instance hashHour :: Hashable Hour where
  hash (Hour h) = hash $ mod h 12
