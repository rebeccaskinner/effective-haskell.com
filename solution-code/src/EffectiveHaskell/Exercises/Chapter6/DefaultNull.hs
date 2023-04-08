module EffectiveHaskell.Exercises.Chapter6.DefaultNull where
import Prelude hiding (null)

class Eq a => Nullable a where
  isNull :: a -> Bool
  isNull = (== null)
  null :: a

instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)

instance Eq a => Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []
