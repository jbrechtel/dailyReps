module Reps20200212 where

import Control.Applicative (Alternative(..))

listFold :: (b -> a -> b) -> b -> [a] -> b
listFold folder first items =
  case items of
    [] -> first
    first' : rest -> listFold folder (folder first first') rest

data Z a = A a | B a | C a

z :: Z a -> a
z (A a) = a
z (B a) = a
z (C a) = a

data Maaybe a = Only a | Not

instance Functor Maaybe where
  fmap fn (Only a) = Only $ fn a
  fmap _ _ = Not

instance Applicative Maaybe where
  pure = Only
  (<*>) mbFn mbV =
    case mbFn of
      Only fn -> fn <$> mbV
      Not -> Not

instance Alternative Maaybe where
  empty = Not
  (<|>) opt1 opt2 =
    case opt1 of
      (Only _) -> opt1
      Not -> opt2

instance Foldable Maaybe where
  foldr foldFn curr mb =
    case mb of
      Not -> curr
      Only a -> foldFn a curr
