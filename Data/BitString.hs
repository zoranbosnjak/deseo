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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.List (unfoldr)
import GHC.Generics

import Test.QuickCheck

-- | Bits data type.
newtype Bits = Bits [Bool] deriving (Generic, Eq, Monoid, Arbitrary)

instance NFData Bits

instance Show Bits where
    show (Bits bb) = "Bits " ++ unwords octets where
        octets = unfoldr getOctet bb
        getOctet [] = Nothing
        getOctet s =
            let (a,b) = splitAt 8 s
                octet = P.take 8 ((bitValue <$> a) ++ repeat '.')
            in Just (octet, b)
        bitValue True = '1'
        bitValue False = '0'

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
index (Bits b) n = b P.!! n

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
zeros n = Bits $ replicate n False

-- | Generate bitstring of given bit length and value,
-- for example:
--
-- >    fromInteger 16 0x1234 == Bits 00010010 00110100
--
fromInteger :: Int -> Integer -> Bits
fromInteger n val = Bits $ reverse $ unfoldr f (n,val) where
    f (0,_) = Nothing
    f (n', val') =
        let (a,b) = val' `divMod` 2
        in Just (bitValue b, (n'-1, a))
    bitValue 0 = False
    bitValue _ = True

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
anySet = P.any id . unpack

-- | Are all bits set?
allSet :: Bits -> Bool
allSet = P.all id . unpack
