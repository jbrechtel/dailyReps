module Reps20200224 where

import Control.Applicative

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll fn item items =
  case items of
    item' : rest -> foldll fn (fn item item') rest
    _            -> item

data NamingExercise item
  = GoodName item

namedThing :: NamingExercise item -> item
namedThing (GoodName item) = item

data Conceivably a
  = Conceivable a
  | Inconceivable

instance Functor Conceivably where
  fmap fn (Conceivable a) = Conceivable $ fn a
  fmap _ _ = Inconceivable

instance Applicative Conceivably where
  pure = Conceivable
  (<*>) cFn cA =
    case cFn of
      Conceivable fn -> fn <$> cA
      _ -> Inconceivable

instance Alternative Conceivably where
  empty = Inconceivable
  (<|>) cA cB =
    case cA of
      Inconceivable -> cB
      _ -> cA

instance Foldable Conceivably where
  foldr fn def conception =
    case conception of
      Conceivable a -> fn a def
      _ -> def

instance Semigroup a => Semigroup (Conceivably a) where
  (<>) a b =
    case (a, b) of
      (Conceivable a', Conceivable b') -> Conceivable $ a' <> b'
      (Conceivable a', _) -> Conceivable a'
      (_, Conceivable b') -> Conceivable b'
      _ -> Inconceivable

instance Monoid a => Monoid (Conceivably a) where
  mempty = Conceivable mempty
