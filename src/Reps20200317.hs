module Reps20200317 where


ffold :: (b -> a -> b) -> b -> [a] -> b
ffold fn seed items =
  case items of
    [] -> seed
    first : rest -> ffold fn (fn seed first) rest

ffoldr :: (a -> b -> b) -> b -> [a] -> b
ffoldr fn seed items =
  case items of
    [] -> seed
    first : rest -> fn first $ ffoldr fn seed rest

data Choice a b
 = OptionA a
 | OptionB b

instance Functor (Choice a) where
  fmap _  (OptionA a) = OptionA a
  fmap fn (OptionB b) = OptionB $ fn b

instance Applicative (Choice a) where
  pure = OptionB
  (<*>) cFn cV =
    case cFn of
      OptionA fn -> OptionA fn
      OptionB fn -> fn <$> cV

instance Foldable (Choice a) where
  foldMap _  (OptionA _) = mempty
  foldMap fn (OptionB b) = fn b

instance (Semigroup a, Semigroup b) => Semigroup (Choice a b) where
  (<>) c1 c2 =
    case (c1, c2) of
      (OptionA a1, OptionA a2) -> OptionA $ a1 <> a2
      (OptionB b1, OptionB b2) -> OptionB $ b1 <> b2

      (OptionA a1, OptionB _)  -> OptionA a1
      (OptionB _,  OptionA a2) -> OptionA a2

instance (Monoid a, Semigroup b) => Monoid (Choice a b) where
  mempty = OptionA mempty
