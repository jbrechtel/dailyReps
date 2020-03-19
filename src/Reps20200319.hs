module Reps20200319 where

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll fn seed items =
  case items of
    [] -> seed
    first : rest -> foldll fn (fn seed first) rest

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr fn seed items =
  case items of
    [] -> seed
    first : rest -> fn first $ foldrr fn seed rest

data E a b = A a | B b

instance Functor (E a) where
  fmap _  (A a) = A a
  fmap fn (B b) = B $ fn b

instance Applicative (E a) where
  pure = B
  (<*>) (A a) _  = A a
  (<*>) (B fn) v = fn <$> v

instance Foldable (E a) where
  foldMap _  (A _) = mempty
  foldMap fn (B b) = fn b

instance (Semigroup a, Semigroup b) => Semigroup (E a b) where
  (<>) (A a1) (A a2) = A $ a1 <> a2
  (<>) (B b1) (B b2) = B $ b1 <> b2
  (<>) (A a1) (B _)  = A a1
  (<>) (B b1) (A _)  = B b1

instance (Monoid a, Semigroup b) => Monoid (E a b) where
  mempty = A mempty
