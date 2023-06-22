{-# LANGUAGE DerivingVia #-}
module EffectiveHaskell.Exercises.Chapter6.DerivingNullable where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a

newtype BasicNullable a = BasicNullable (Maybe a)
  deriving stock Show

instance Nullable (BasicNullable a) where
  isNull (BasicNullable Nothing) = True
  isNull _ = False
  null = BasicNullable Nothing

newtype TransitiveNullable a = TransitiveNullable (Maybe a)
  deriving stock Show

instance Nullable a => Nullable (TransitiveNullable a) where
  isNull (TransitiveNullable Nothing) = True
  isNull (TransitiveNullable (Just a)) = isNull a
  null = TransitiveNullable Nothing

instance Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []

newtype OptionalString = OptionalString { getString :: Maybe String }
  deriving stock Show
  deriving Nullable via BasicNullable String

newtype OptionalNonEmptyString = OptionalNonEmptyString { getNonEmptyString :: Maybe String }
  deriving stock Show
  deriving Nullable via TransitiveNullable String
