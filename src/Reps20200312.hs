module Reps20200312 where

ffoldl :: (b -> a -> b) -> b -> [a] -> b
ffoldl fn seed items =
  case items of
    [] -> seed
    first : rest -> ffoldl fn (fn seed first) rest

ffoldr :: (a -> b -> b) -> b -> [a] -> b
ffoldr fn seed items =
  case items of
    [] -> seed
    first : rest -> fn first $ ffoldr fn seed rest

data EitherOr a b = This a | That b

instance Functor (EitherOr a) where
  fmap _  (This a) = This a
  fmap fn (That b) = That $ fn b

instance Applicative (EitherOr a) where
  pure = That
  (<*>) eFn eV =
    case eFn of
      This a -> This a
      That b -> b <$> eV

instance Foldable (EitherOr a) where
  foldMap fn eV =
    case eV of
      This _ -> mempty
      That b -> fn b

instance (Semigroup a, Semigroup b) => Semigroup (EitherOr a b) where
  (<>) e1 e2 =
    case (e1, e2) of
      (This a1, This a2) -> This $ a1 <> a2
      (This a1, That _) -> This a1
      (That b1, That b2) -> That $ b1 <> b2
      (That b1, This _) -> That b1

instance (Monoid a, Monoid b) => Monoid (EitherOr a b) where
  mempty = This mempty
