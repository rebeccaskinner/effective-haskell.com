{-# LANGUAGE DefaultSignatures #-}
module EffectiveHaskell.BonusContent.Chapter6.DefaultSignatures where

import Data.Word

-- From the 'containers' package
import Data.Set (Set)
import qualified Data.Set as Set

-- From the 'hashable' package
import Data.Hashable

-- From the 'vectors' package
import Data.Vector (Vector)
import qualified Data.Vector as Vector

class Contains t where
  checkNumber :: Int -> t Int -> Bool
  default checkNumber :: Foldable t => Int -> t Int -> Bool
  checkNumber n = foldr (\element found -> (element == n) || found) False

instance Contains Set where
  checkNumber = Set.member

instance Contains []

data BloomFilter = BloomFilter
  { bloomFilterBits :: Vector Word8
  , bloomFilterSize :: Word16
  }
  deriving (Eq, Show)

emptyBloomFilter :: Word16 -> BloomFilter
emptyBloomFilter size = BloomFilter emptyBitSet size
  where
    emptyBitSet = Vector.replicate 0 $ fromIntegral size

insertBloomFilter :: Int -> BloomFilter -> BloomFilter
insertBloomFilter n =
