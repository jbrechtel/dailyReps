module Reps20200228 where

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL fn h items =
  case items of
    [] -> h
    h' : rest -> foldL fn (fn h h') rest

data Choice a b = OptionA a | OptionB b

instance Functor (Choice a) where
  fmap fn (OptionB b) = OptionB $ fn b
  fmap _ (OptionA a) = OptionA a

instance Applicative (Choice a) where
  pure = OptionB
  (<*>) cFn option =
    case cFn of
      OptionA fn -> OptionA fn
      OptionB fn -> fn <$> option

instance Foldable (Choice a) where
  foldr fn h opt =
    case opt of
      OptionA _  -> h
      OptionB b  -> fn b h

instance Semigroup b => Semigroup (Choice a b) where
  (<>) optB optB' =
    case (optB, optB') of
      (OptionB b, OptionB b') -> OptionB $ b <> b'
      (OptionB b, OptionA _)  -> OptionB b
      (OptionA _, OptionB b') -> OptionB b'
      (OptionA a, OptionA _)  -> OptionA a --UHHH well this is bad

instance (Semigroup b, Monoid a) => Monoid (Choice a b) where
  mempty = OptionA mempty --this is also bad
