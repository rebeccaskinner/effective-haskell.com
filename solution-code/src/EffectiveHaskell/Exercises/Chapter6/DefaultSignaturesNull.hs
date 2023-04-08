{-# LANGUAGE DefaultSignatures #-}
module EffectiveHaskell.Exercises.Chapter6.DefaultSignaturesNull where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool

  default isNull :: Eq a => a -> Bool
  isNull = (== null)
  null :: a

instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)

instance Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []
