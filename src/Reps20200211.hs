module Reps20200211 where

import Control.Applicative (Alternative(..))

foldList :: (b -> a -> b) -> b -> [a] -> b
foldList foldFn curr list =
  case list of
    [] -> curr
    start : rest -> foldList foldFn (foldFn curr start) rest

data Things a
  = Stuff a
  | Junk a
  | Emptiness


unwrap :: Things a -> Maybe a
unwrap things =
  case things of
    Stuff a -> Just a
    Junk a -> Just a
    Emptiness -> Nothing

data Potential a
  = Realized a
  | Unimproved

instance Functor Potential where
  fmap fn p =
    case p of
      Realized a -> Realized $ fn a
      Unimproved -> Unimproved

instance Applicative Potential where
  pure = Realized
  (<*>) potentialFn potential =
    case potentialFn of
      Realized fn -> fn <$> potential
      Unimproved -> Unimproved

instance Alternative Potential where
  empty = Unimproved
  (<|>) potential potential' =
    case (potential, potential') of
      (Realized a, Realized _) -> Realized a
      (Realized a, Unimproved) -> Realized a
      (Unimproved, Realized a) -> Realized a
      (Unimproved, Unimproved) -> Unimproved

instance Foldable Potential where
  foldr foldFn curr potential =
    case potential of
      Realized a -> foldFn a curr
      Unimproved -> curr
