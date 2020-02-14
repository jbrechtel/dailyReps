module Reps20200214 where

import Control.Applicative

lfoldl :: (b -> a -> b) -> b -> [a] -> b
lfoldl fn zero items =
  case items of
    [] -> zero
    item : rest -> lfoldl fn (fn zero item) rest

data Foo
  = Bar
  | Baz


data Buz b
  = Buz b
  | Biz

blaz :: Buz b -> Maybe b
blaz buz =
  case buz of
    Buz b -> Just b
    Biz -> Nothing

instance Functor Buz where
  fmap fn (Buz b) = Buz $ fn b
  fmap _ Biz = Biz

instance Applicative Buz where
  pure = Buz
  (<*>) buzFn buzVal =
    case buzFn of
      Buz fn -> fn <$> buzVal
      Biz -> Biz

instance Alternative Buz where
  empty = Biz
  (<|>) buzVal buzVal' =
    case buzVal of
      (Buz _) -> buzVal
      _ -> buzVal'

instance Foldable Buz where
  foldMap fn buzVal =
    case buzVal of
      Buz a -> fn a
      _ -> mempty
