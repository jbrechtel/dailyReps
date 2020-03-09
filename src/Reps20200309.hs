module Reps20200309 where

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

data Ether a b
  = Lft a
  | Rght b

instance Functor (Ether a) where
  fmap _ (Lft a) = Lft a
  fmap fn (Rght b) = Rght $ fn b

instance Applicative (Ether a) where
  pure = Rght
  (<*>) eFn eV =
    case eFn of
      Lft a -> Lft a
      Rght b -> b <$> eV

instance Foldable (Ether a) where
  foldr fn seed items =
    case items of
      Lft _ -> seed
      Rght b -> fn b seed

instance (Semigroup a, Semigroup b) => Semigroup (Ether a b) where
  (<>) e1 e2 =
    case (e1, e2) of
      (Lft a1, Lft a2) -> Lft $ a1 <> a2
      (Lft a1, _) -> Lft a1
      (Rght b1, Rght b2) -> Rght $ b1 <> b2
      (Rght b1, Lft _) -> Rght b1

instance (Semigroup a, Monoid b) => Monoid (Ether a b) where
  mempty = Rght mempty
