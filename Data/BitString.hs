----------------
-- |
-- Module       :  Data.BitString
-- Copyright:   (c) 2015-2016 Zoran Bošnjak
--              (c) 2015-2016 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides simple bitstring manipulation library.
-- The implementation is based on lists - [Bool].
--
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Data.BitString as B
--
-- Examples:
--
-- >    >>> B.pack [True, False, False]
-- >    Bits 100.....
-- >    >>> B.take 2 $ B.pack [True, False, False]
-- >    Bits 10......
-- >    >>> B.fromInteger 16 0x0102
-- >    Bits 00000001 00000010
-- >    >>> B.toUIntegral $ B.pack [True, False]
-- >    2
--

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.BitString (

    Bits

    -- * Bits creation
    , pack

    -- * Util functions
    , length
    , take
    , takeMaybe
    , drop
    , dropMaybe
    , index
    , zeros
    , unpack
    , null
    , checkAligned
    , complement
    , anySet, allSet

    -- * Convert functions
    , fromInteger
    , toSIntegral, toUIntegral
    , fromByteString
    , toByteString
) where

import Control.DeepSeq
import Control.Monad
import qualified Prelude as P
import Prelude 
    hiding (length, any, fromInteger, toInteger, take, drop, null, (!!))
import qualified Data.ByteString as S
import Data.Word
import qualified Data.Bits as B
import Data.List (foldl1')
import GHC.Generics

import Test.QuickCheck

-- | Bits data type.
data Bits = Bits ![Bool] deriving (Generic, Eq)

instance NFData Bits

instance Show Bits where
    show (Bits bb) = "Bits " ++ disp bb where
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
    mempty = Bits []
    Bits a `mappend` Bits b = Bits (a `mappend` b)

instance Arbitrary Bits where
    arbitrary = liftM Bits arbitrary

-- | Calculate length of bitstring.
length :: Bits -> Int
length (Bits a) = P.length a

-- | Take first 'n' bits.
take :: Int -> Bits -> Bits
take n (Bits a) = Bits $ P.take n a

-- | Take first 'n' bits (if available).
takeMaybe :: Int -> Bits -> Maybe Bits
takeMaybe n b
    | length b < n = Nothing
    | otherwise = Just $ take n b

-- | Drop first 'n' bits.
drop :: Int -> Bits -> Bits
drop n (Bits a) = Bits $ P.drop n a

-- | Drop first 'n' bits (if available)
dropMaybe :: Int -> Bits -> Maybe Bits
dropMaybe n b
    | length b < n = Nothing
    | otherwise = Just $ drop n b

-- | Return bit at given index.
index :: Bits -> Int -> Bool
index (Bits b) n = (P.!!) b n

-- | Convert from list of Bool values to Bits
pack :: [Bool] -> Bits
pack = Bits

-- | Convert from Bits to list of Bool values
unpack :: Bits -> [Bool]
unpack (Bits b) = b

-- | Is empty bitstring?
null :: Bits -> Bool
null (Bits b) = P.null b

-- | Return 'n' bits of all zero values.
zeros :: Int -> Bits
zeros n = fromInteger n (0::Integer)

-- | Generate bitstring of given bit length and value,
-- for example:
--
-- >    bits 16 0x1234
--
fromInteger :: Int -> Integer -> Bits
fromInteger n val = Bits . map toBool $ f n val [] where
    f 0 _ acc = acc
    f n' val' acc = f (n'-1) a (b:acc) where
        (a,b) = divMod val' 2
    toBool 0 = False
    toBool _ = True

-- | If byte aligned, return Just value, else 'Nothing'.
checkAligned :: Bits -> Maybe Bits
checkAligned b = case (length b `mod` 8) of
    0 -> Just b
    _ -> Nothing

-- False=0, True=1
boolVal :: Num a => Bool -> a
boolVal False = 0
boolVal True = 1

-- | Convert bits to signed number.
toSIntegral :: Num a => Bits -> a
toSIntegral b
    | null b = 0
    | (head . unpack $ b) == False = toUIntegral b
    | otherwise = -((toUIntegral $ complement b)+1)

-- | Convert bits to unsigned number.
toUIntegral :: Num a => Bits -> a
toUIntegral (Bits b) = sum . zipWith (*) factors . map boolVal . reverse $ b 
  where
    factors = map (2^) ([0..]::[Int])

-- | Convert bytestring to Bits.
fromByteString :: S.ByteString -> Bits
fromByteString = Bits . concatMap octet . S.unpack where
    octet w = map (B.testBit w) [7,6..0]

-- | Convert Bits to bytestring.
toByteString :: Bits -> Maybe S.ByteString
toByteString bs = do
    (Bits s) <- checkAligned bs
    return . S.pack . map toWord . break8 $ s where
        break8 :: [a] -> [[a]]
        break8 [] = [] 
        break8 s = a : break8 b where (a,b) = splitAt 8 s
        toWord :: [Bool] -> Word8
        toWord s = sum $ 
            zipWith (*) (map (2^) ([7,6..0]::[Int])) (map boolVal s)

-- | Reverse all the bits.
complement :: Bits -> Bits
complement (Bits b) = Bits $ map not b

-- | Is any bit set?
anySet :: Bits -> Bool
anySet = P.any (==True) . unpack

-- | Are all bits set?
allSet :: Bits -> Bool
allSet = P.all (==True) . unpack

