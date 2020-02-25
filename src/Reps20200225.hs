module Reps20200225 where

import Control.Applicative

ffoldl :: (b -> a -> b) -> b -> [a] -> b
ffoldl fn top items =
  case items of
    first : rest -> ffoldl fn (fn top first) rest
    _ -> top


data Has a
  = Yes a
  | No

instance Functor Has where
  fmap fn has =
    case has of
      Yes a -> Yes $ fn a
      No -> No

instance Applicative Has where
  pure = Yes
  (<*>) fnA a =
    case fnA of
      Yes fn -> fn <$> a
      No -> No

instance Alternative Has where
  empty = No
  (<|>) has has' =
    case has of
      Yes _ -> has
      _ -> has'

instance Foldable Has where
  foldMap fn has =
    case has of
      Yes a -> fn a
      No -> mempty

instance Semigroup a => Semigroup (Has a) where
  (<>) hasA hasA' =
    case (hasA, hasA') of
      (Yes a, Yes a') -> Yes $ a <> a'
      (Yes a, No)     -> Yes a
      (No, Yes a')    -> Yes a'
      (No, No)        -> No

instance Monoid a => Monoid (Has a) where
  mempty = No
