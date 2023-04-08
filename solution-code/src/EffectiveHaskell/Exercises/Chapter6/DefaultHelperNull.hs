module EffectiveHaskell.Exercises.Chapter6.DefaultHelperNull where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a

isNullHelper :: (Eq a, Nullable a) => a -> Bool
isNullHelper = (== null)

instance (Eq a, Nullable a) => Nullable (Maybe a) where
  isNull = isNullHelper
  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)

instance Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []
