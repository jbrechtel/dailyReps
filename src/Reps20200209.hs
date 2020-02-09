module Reps20200209 where

fmapList :: (a -> b) -> [a] -> [b]
fmapList fn list =
  case list of
    [] -> []
    a : rest -> fn a : fmapList fn rest

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe fn mb =
  case mb of
    Nothing -> Nothing
    Just a -> Just $ fn a

data Foo
  = Foo
  | Bar
  | Baz

fooStr :: Foo -> String
fooStr foo =
  case foo of
    Foo -> "foo"
    Bar -> "bar"
    Baz -> "baz"

data Vehicle a
  = Bicycle a
  | Motorcycle a
  | Boat a

unwrapVehicle :: Vehicle a -> a
unwrapVehicle vehicle =
  case vehicle of
    Bicycle a -> a
    Motorcycle a -> a
    Boat a -> a

data MaybeJames a
  = JustJames a
  | NothingJames

instance Functor MaybeJames where
  fmap fn james =
    case james of
      JustJames a -> JustJames $ fn a
      NothingJames -> NothingJames

data ListJames a
  = SingletonList a
  | NestedList a (ListJames a)

instance Functor ListJames where
  fmap fn list =
    case list of
      SingletonList a -> SingletonList $ fn a
      NestedList a rest -> NestedList (fn a) $ fmap fn rest
