
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
  , toUnsigned
  , pack
  , unpack
  , checkAligned
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

-- shift ByteString by number of bits
shiftByteStringR :: Int -> S.ByteString -> [Word8]
shiftByteStringR n b 
    | n < 0 = error "expect positive shift"
    | otherwise = 
        adjust . rotate . shift1 $ b where
        (byteShift, bitShift) = divMod n 8
        shift1 = S.take (S.length b - byteShift)
        rotate = S.map (flip B.rotateR $ bitShift)
        m1 = B.shiftR (0xff :: Word8) bitShift -- mask1
        m2 = B.complement m1            -- mask2
        v1 = S.map (m1 B..&.)
        v2 = S.cons 0 . S.init . S.map (m2 B..&.)
        adjust x = S.zipWith (B..|.) (v1 x) (v2 x)

bytesRequired numOfBits = a + if b==0 then 0 else 1 where (a,b) = divMod numOfBits 8

-- convert to compact form
toMinList :: Bits -> [Word8]
toMinList (Bytes x) = S.unpack x
toMinList (Slice x a b) = mask . reduce2 . shiftByteStringR n . reduce1 $ x where
    total = b-a
    n = ((8 - (b `mod` 8)) `mod` 8) -- required shift
    reduce1 = S.take (i2-i1) . S.drop i1 -- shorter bytestring stage 1
    i1 = a `div` 8
    i2 = bytesRequired b -- second word8 index
    reduce2 y = P.drop ((P.length y) - (bytesRequired total)) y
    mask [] = []
    mask (x:xs) = ((B..&.) mask1 x):xs
    mask1 = B.complement $ B.shiftL 0xff (total `mod` 8)

-- convert bits to unsigned integer
toUnsigned :: Num a => Bits -> a
toUnsigned bs = let words = reverse . map P.fromIntegral . toMinList $ bs
                    factors = map (256^) [0..]
                    elements = zipWith (*) words factors
                in foldr (+) 0 elements

instance Eq Bits where
    a == b = (length a == length b) && (toMinList a == toMinList b)

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

-- is byte aligned
checkAligned :: Bits -> Maybe Bits
checkAligned bs@(Bytes x) = Just bs
checkAligned bs@(Slice x a b)
    | (a `mod` 8) == 0 = Just bs
    | otherwise = Nothing

