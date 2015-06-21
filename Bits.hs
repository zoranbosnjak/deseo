
{-
Bit manipulation

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Bits
( Bits
  , length
  , takeBits
  , toBits
) where

import Prelude hiding (length)
import Data.Monoid
import qualified Data.ByteString as S

data Bits = Bytes S.ByteString
            | StartOf S.ByteString Int
            | Slice S.ByteString Int Int
            deriving (Show)

length :: Bits -> Int
length (Bytes x) = (S.length x)*8
length (StartOf _ n) = n
length (Slice _ a b) = b-a

-- shift bits if necessary to start at 0 offset
toStartOf :: Bits -> (S.ByteString, Int)
toStartOf val@(Bytes x) = (x, (length val))
toStartOf (StartOf x n) = (x, n)
toStartOf (Slice x a b) = undefined

-- take first n bits
takeBits :: Int -> Bits -> Bits
takeBits = undefined

instance Monoid Bits where
    mempty = Bytes S.empty

    mappend (Bytes a) (Bytes b) = Bytes (a `mappend` b)
    mappend b1@(Bytes a) b2@(Slice b x y) = undefined
    mappend b1 b2 = undefined

toBits :: [Bool] -> Bits
toBits list = undefined

toList :: Bits -> [Bool]
toList bits = undefined

