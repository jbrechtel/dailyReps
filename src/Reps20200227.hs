module Reps20200227 where

import Control.Applicative

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll fn seed items =
  case items of
    [] -> seed
    first : rest -> foldll fn (fn seed first) rest

data MMaybe a = JJust a | NNothing

instance Functor MMaybe where
  fmap fn (JJust a) = JJust $ fn a
  fmap _ NNothing = NNothing

instance Applicative MMaybe where
  pure = JJust
  (<*>) mFn mA =
    case mFn of
      JJust fn -> fn <$> mA
      NNothing  -> NNothing

instance Alternative MMaybe where
  empty = NNothing
  (<|>) mA mB =
    case mA of
      JJust _  -> mA
      NNothing -> mB

instance Foldable MMaybe where
  foldMap fn mA =
    case mA of
      JJust a -> fn a
      _ -> mempty

instance Monoid a => Semigroup (MMaybe a) where
  (<>) mA mB =
    case (mA, mB) of
      (JJust a, JJust b)   -> JJust $ a <> b
      (JJust a, NNothing)  -> JJust a
      (NNothing, JJust b)  -> JJust b
      (NNothing, NNothing) -> NNothing

instance Monoid a => Monoid (MMaybe a) where
  mempty = NNothing
