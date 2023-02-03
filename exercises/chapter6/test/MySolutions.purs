module Test.MySolutions where

import Prelude

import Data.Array (concat, cons)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap, over2)
import Data.Ord (Ordering(..))
import Data.Semigroup (append)
import Data.Show.Generic (genericShow)

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
