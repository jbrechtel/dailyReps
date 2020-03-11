module Reps20200311 where

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

data Neither a b = NotLeft a | NotRight b

instance Functor (Neither a) where
  fmap fn neither =
    case neither of
      NotLeft a -> NotLeft a
      NotRight b -> NotRight $ fn b

instance Applicative (Neither a) where
  pure = NotRight
  (<*>) nFn nV =
    case nFn of
      NotLeft a -> NotLeft a
      NotRight fn -> fn <$> nV

instance Foldable (Neither a) where
  foldr fn seed items =
    case items of
      NotLeft _ -> seed
      NotRight b -> fn b seed


instance (Semigroup a, Semigroup b) => Semigroup (Neither a b) where
  (<>) n1 n2 =
    case (n1, n2) of
      (NotLeft a1, NotLeft a2) -> NotLeft $ a1 <> a2
      (NotLeft _, NotRight b2) -> NotRight b2
      (NotRight b1, NotRight b2) -> NotRight $ b1 <> b2
      (NotRight b1, NotLeft _) -> NotRight b1

instance (Monoid a, Semigroup b) => Monoid (Neither a b) where
  mempty = NotLeft mempty
