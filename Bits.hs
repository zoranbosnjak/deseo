
{-
Bit manipulation

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Bits
( Bits(..)
  , length
  , take
  , drop
  , zeros
  , index
  , pack
  , unpack
) where

import qualified Prelude as P
import Prelude hiding (length, any, toInteger, take, drop)
import Data.Monoid
import qualified Data.ByteString as S
import Data.Word
import qualified Data.Bits as B
import Data.List (foldl1')

data Bits = Bytes S.ByteString
            | Slice S.ByteString Int Int

instance Show Bits where
    show b = "Bits " ++ disp (unpack b) where
        disp [] = ""
        disp x = foldl1' (\a b -> a++" "++b) . spl . map f $ x
        f True = '1'
        f False = '0'
        spl [] = []
        spl s = 
            let (a,b) = splitAt 8 s
            in (fill a):(spl b)
        fill a = P.take 8 (a++repeat '.')

instance Monoid Bits where
    mempty = Bytes S.empty

    mappend (Bytes a) (Bytes b) = Bytes (a `mappend` b)
    mappend b1@(Bytes a) b2@(Slice b x y) = undefined
    mappend b1 b2 = undefined

-- shift bits, return (bytestring, remainder, size)
strip :: Bits -> (S.ByteString, Word8, Int)
strip bs@(Bytes x) = (x, 0, length bs)
strip bs@(Slice x a b) = undefined

instance Eq Bits where
    a == b = (strip a) == (strip b)

-- calculate bits length
length :: Bits -> Int
length (Bytes x) = (S.length x)*8
length (Slice _ a b) = b-a

-- take first n bits
take :: Int -> Bits -> Bits
take n (Bytes x)
    | b == 0 = Bytes (S.take a x)
    | otherwise = Slice x 0 n where
        (a,b) = divMod n 8
take n (Slice x a b) = Slice x a (a+n)

-- drop first n bits
drop :: Int -> Bits -> Bits
drop n bs@(Bytes x)
    | b == 0 = Bytes (S.drop a x)
    | otherwise = Slice x n (length bs) where
        (a,b) = divMod n 8
drop n (Slice x a b) = Slice x (a+n) b

-- generate bitstring of all zeros
zeros :: Int -> Bits
zeros n
    | b == 0 = Bytes . S.pack . replicate a $ 0
    | otherwise = Slice (S.pack . replicate (a+1) $ 0) 0 n where
        (a,b) = divMod n 8

-- get bit value at given index
index :: Bits -> Int -> Bool
index bs n 
    | (n>=length bs) = error "index out of range"
    | otherwise = B.testBit (word bs n) (offset bs n) where
        word (Bytes x) n = S.index x (div n 8)
        word (Slice x a b) n = S.index x (div (a+n) 8)
        offset (Bytes _) n = 7 - (mod n 8)
        offset (Slice x a b) n = 7 - (mod (a+n) 8)

pack :: [Bool] -> Bits
pack = undefined

unpack :: Bits -> [Bool]
unpack b = [index b i | i<-[0..(length b)-1]]

