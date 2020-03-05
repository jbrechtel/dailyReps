module Reps20200304 where

pholdl :: (b -> a -> b) -> b -> [a] -> b
pholdl fn first items =
  case items of
    [] -> first
    firstItem : rest -> pholdl fn (fn first firstItem) rest

data Pick a b =
  This a | That b

instance Functor (Pick a) where
  fmap _ (This a) = This a
  fmap fn (That b) = That $ fn b

instance Applicative (Pick a) where
  pure = That
  (<*>) pickFn pickV =
    case pickFn of
      This a -> This a
      That fn -> fn <$> pickV

instance Foldable (Pick a) where
  foldr fn agg pick =
    case pick of
      This _ -> agg
      That b -> fn b agg

instance (Semigroup a, Semigroup b) => Semigroup (Pick a b) where
  (<>) pick pick' =
    case (pick, pick') of
      (This a, This a') -> This $ a <> a'
      (This a, That _) -> This a
      (That b, That b') -> That $ b <> b'
      (That b, This _) -> That b

instance (Semigroup a, Monoid b) => Monoid (Pick a b) where
  mempty = That mempty
