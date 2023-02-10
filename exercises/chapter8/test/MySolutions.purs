module Test.MySolutions where

import Prelude

import Data.Array (filterA, foldM, head, nub, sort, tail)
import Data.Foldable (sum)
import Data.List (List(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Control.Monad.ST.Ref (modify, new, read)
import Control.Monad.ST (ST, for, run)

third :: forall a. Array a -> Maybe a
third arr = do
  skip1 <- tail arr
  skip2 <- tail skip1
  head skip2

-- third a = helper 2 a
--   where 
--     helper n arr | n > 0 = case tail arr of
--           Just tl -> helper (n-1) tl
--           Nothing -> Nothing
--     helper _ arr = case head arr of 
--           Just hd -> Just hd
--           Nothing -> Nothing

possibleSums :: Array Int -> Array Int
possibleSums coins = sort $ nub $ map sum (powerset coins)
  where 
    powerset :: Array Int -> Array (Array Int)
    powerset arr = filterA (const [true,false]) arr

possibleSums' :: Array Int -> Array Int
possibleSums' xs = foldM (\acc i -> [ acc, acc + i ]) 0 xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM p (Cons hd tl) = do
    pa <- p hd
    tl' <- filterM p tl
    if pa then
      pure (Cons hd tl')
    else 
      pure tl'

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure (a/b)

simulate :: forall r. Number -> Number -> Int -> ST r Number
simulate x0 v0 time = do
  ref <- new { x: x0, v: v0 }
  for 0 (time * 1000) \_ ->
    modify
      ( \o ->
          { v: o.v - 9.81 * 0.001
          , x: o.x + o.v * 0.001
          }
      )
      ref
  final <- read ref
  pure final.x

simulate' :: Number -> Number -> Int -> Number
simulate' x0 v0 time = run (simulate x0 v0 time)

-- 1 - 1 + 1 - 1 ...  == PI/4
---    3   5   7 

estimatePi :: Int -> Number
estimatePi k = 4.0 * run (est k)

est :: forall r. Int -> ST r Number
est n = do
   ref <- new { n': n, bottom: 1.0, acc: 1.0 }
   for 1 n \_ -> 
     modify 
       ( \o ->
         { n': o.n' + 1,
           bottom: o.bottom + 2.0,
           acc: if o.n' `mod` 2 == 0 then o.acc - (1.0/(o.bottom + 2.0)) else o.acc + (1.0/(o.bottom + 2.0))
           }  
       )
       ref
   final <- read ref
   pure final.acc




