module Reps20200221 where

import Control.Applicative (Alternative(..))

foldyListy :: (b -> a -> b) -> b -> [a] -> b
foldyListy fn first items =
  case items of
    [] -> first
    second : rest -> foldyListy fn (fn first second) rest

data Thing a
  = Incarnation a
  | Abomination a a

bestThing :: Ord a => Thing a -> a
bestThing thing =
  case thing of
    Incarnation a -> a
    Abomination a a' -> max a a'

data Ostensibly a
  = Truth a
  | Lie

instance Functor Ostensibly where
  fmap f ost =
    case ost of
      Truth a -> Truth $ f a
      Lie -> Lie

instance Applicative Ostensibly where
  pure = Truth
  (<*>) fnA fA =
    case fnA of
      Truth a -> a <$> fA
      Lie -> Lie

instance Alternative Ostensibly where
  empty = Lie
  (<|>) a a' =
    case a of
      Truth _ -> a
      Lie -> a'

instance Foldable Ostensibly where
  foldr fn cur ost =
    case ost of
      Truth a -> fn a cur
      Lie -> cur

instance Semigroup a => Semigroup (Ostensibly a) where
  (<>) ost ost' =
    case (ost, ost') of
      (Truth a, Truth a') -> Truth $ a <> a'
      (Truth a, Lie) -> Truth a
      (Lie, Truth a) -> Truth a
      (Lie, Lie) -> Lie

instance Monoid a => Monoid (Ostensibly a) where
  mempty = Lie
