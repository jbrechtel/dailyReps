module Reps20200210 where

fmap' :: (a -> b) -> [a] -> [b]
fmap' fn items =
  case items of
    [] -> []
    first : rest -> fn first : fmap' fn rest

fmap'' :: (a -> b) -> Maybe a -> Maybe b
fmap'' fn mb =
  case mb of
    Nothing -> Nothing
    Just a -> Just $ fn a

data Feeling
  = Happy
  | Sad
  | Confused
  | Angry
  | Calm
  | Jealous
  | Scared

feelingString :: Feeling -> String
feelingString f =
  case f of
    Happy -> ":)"
    Sad -> ":("
    Confused -> ":/"
    Angry -> ">:|"
    Calm -> ":|"
    Jealous -> "8-()"
    Scared -> "=-|"

data Frivolous a = Frivolous a

important :: Frivolous a -> a
important (Frivolous a) = a

data Possibly a
  = Realized a
  | Impossible

instance Functor Possibly where
  fmap fn possibly =
    case possibly of
      Realized a -> Realized $ fn a
      Impossible -> Impossible

instance Applicative Possibly where
  pure = Realized
  (<*>) possiblyFn possibly =
    case possiblyFn of
      Realized fn -> fn <$> possibly
      Impossible  -> Impossible
