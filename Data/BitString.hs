----------------
-- |
-- Module       :  Data.BitString
-- Copyright:   (c) 2015-2016 Zoran Bošnjak
--              (c) 2015-2016 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides simple bitstring manipulation library.
--
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Data.BitString as B
--
-- Examples:
--
-- >    > b = pack [True, False, False]
-- >    > b
-- >    Bits 100.....
-- >    > take 2 b
-- >    Bits 10......
--
-- >    > fromInt 16 0x0102
-- >    Bits 00000001 00000010
--
-- >    > toUnsigned $ pack [True, False]
-- >    2
--
-- >    > toSigned $ pack [True, False]
-- >    -2
--
-- >    > b1 = pack [True]
-- >    > b2 = pack [True, False]
-- >    > b1 == b2
-- >    False
-- >    > toByteString b1 == toByteString b2
-- >    True
-- >    > toByteString (makeAligned b1) == toByteString (makeAligned b2)
-- >    False
-- >    > makeAligned b1
-- >    Bits 00000001
-- >    > makeAligned b2
-- >    Bits 00000010

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BitString (

    Bits
    , shrinkBits
    , showBits

    -- * Bits creation
    , pack

    -- * Util functions
    , length
    , take
    , takeMaybe
    , drop
    , dropMaybe
    , testBit
    , zeros
    , unpack
    , null
    , isAligned
    , makeAligned
    , complement
    , anySet, allSet
    , packWord8, unpackWord8

    -- * Convert functions
    , fromIntegral, fromInteger, fromInt
    , toSigned, toUnsigned
    , fromByteString
    , toByteString
    , toMaybeByteString
) where

import Control.DeepSeq
import qualified Prelude as P
import Prelude
    hiding (length, any, fromIntegral, fromInteger, toInteger, take, drop, null, (!!))
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Data.Word
import qualified Data.Bits as B
import Data.Maybe
import Control.Monad
import GHC.Generics
import Test.QuickCheck

-- | There are multiple (isomorphic) variants of bitstring, to avoid unnecessary conversion.
data Bits
    = BitsNoOffset BS.ByteString Int        -- data length
    | BitsWithOffset BS.ByteString Int Int  -- data offset length
    | BitBools [Bool]                       -- list of bits
    | BitsConcat [Bits]                     -- concatinated bits
    deriving (Generic, Show)

-- | Create more compact form of Bits, to reduce further computations.
shrinkBits :: Bits -> Bits
shrinkBits (BitsNoOffset _ 0) = mempty
shrinkBits (BitsNoOffset bs n) =
    let bytesTaken = case divMod n 8 of
            (a,0) -> a
            (a,_) -> succ a
    in BitsNoOffset (BS.take bytesTaken bs) n
shrinkBits (BitsWithOffset _ _ 0) = mempty
shrinkBits (BitsWithOffset bs offset n) =
    let bytesDropped = div offset 8
        bitsDropped = bytesDropped * 8
        offset' = offset - bitsDropped
        bytesTaken = case divMod (n + offset') 8 of
            (a,0) -> a
            (a,_) -> succ a
        bs' = BS.take bytesTaken $ BS.drop bytesDropped bs
    in case offset' of
        0 -> BitsNoOffset bs' n
        _ -> BitsWithOffset bs' offset' n
shrinkBits (BitBools lst) = BitBools lst
shrinkBits (BitsConcat lst) = case (fmap shrinkBits lst) of
    [] -> mempty
    (single:[]) -> single
    multiple -> BitsConcat $ P.filter (not . null) multiple

-- | Convert from list of Bool values to Bits
pack :: [Bool] -> Bits
pack = BitBools

-- False=0, True=1
boolVal :: Num a => Bool -> a
boolVal False = 0
boolVal True = 1

packWord8 :: [Bool] -> Word8
packWord8 s = sum $
    zipWith (*) (map (2^) ([7,6..0]::[Int])) (map boolVal s)

unpackWord8 :: Word8 -> [Bool]
unpackWord8 x = fmap (B.testBit x) [7,6..0]

-- | Convert from Bits to list of Bool values
unpack :: Bits -> [Bool]
unpack = go . shrinkBits
  where
    go (BitsNoOffset bs ln) =
        let bits = mconcat $ fmap unpackWord8 $ BS.unpack bs
        in P.take ln bits
    go (BitsWithOffset bs offset ln) =
        let bits = mconcat $ fmap unpackWord8 $ BS.unpack bs
        in P.take ln $ P.drop offset $ bits
    go (BitBools lst) = lst
    go (BitsConcat lst) = mconcat $ fmap go lst

instance Eq Bits where
    b1 == b2 = unpack b1 == unpack b2

-- | Show bits in more readable format.
showBits :: Bits -> String
showBits value = "Bits " ++ unwords octets
      where
        octets = unfoldr getOctet (unpack value)
        getOctet [] = Nothing
        getOctet s =
            let (a,b) = splitAt 8 s
                octet = P.take 8 ((bitValue <$> a) ++ repeat '.')
            in Just (octet, b)
        bitValue True = '1'
        bitValue False = '0'

instance Semigroup Bits where
    BitsConcat lst1 <> BitsConcat lst2 = BitsConcat (lst1 <> lst2)
    BitsConcat lst1 <> b2 = BitsConcat (lst1 ++ [b2])
    b1 <> BitsConcat lst2 = BitsConcat (b1:lst2)
    b1 <> b2 = BitsConcat [b1,b2]

instance Monoid Bits where
    mempty = BitBools []

instance Arbitrary Bits where
    arbitrary = oneof
        [ do
            bs <- randBs
            n <- arbitrary `suchThat` (\n -> n >= 0 && n <= (BS.length bs * 8))
            return $ BitsNoOffset bs n
        , do
            bs <- randBs
            let ln = BS.length bs * 8
            offset <- arbitrary `suchThat` (\n -> n >= 0 && n <= ln)
            n <- arbitrary `suchThat` (\n -> n >= 0 && offset + n <= ln)
            return $ BitsWithOffset bs offset n
        , BitBools <$> arbitrary
        , do
            -- exclude BitsConcat from the list,
            -- to prevent generation of infinite (nested) structure
            let isValid = \case
                    BitsConcat _ -> False
                    _ -> True
            BitsConcat <$> listOf (arbitrary `suchThat` isValid)
        ]
      where
        randBs = BS.pack <$> arbitrary

instance NFData Bits

-- | Length of a bitstring.
length :: Bits -> Int
length (BitsNoOffset _ n) = n
length (BitsWithOffset _ _ n) = n
length (BitBools lst) = P.length lst
length (BitsConcat lst) = sum $ fmap length lst

-- | Take first 'n' bits (if available).
takeMaybe :: Int -> Bits -> Maybe Bits
takeMaybe 0 _ = Just mempty
takeMaybe n b = do
    guard $ length b >= n
    Just $ case b of
        BitsNoOffset bs _ln -> BitsNoOffset bs n
        BitsWithOffset bs offset _ln -> BitsWithOffset bs offset n
        BitBools lst -> BitBools $ P.take n lst
        BitsConcat lst -> BitsConcat (go 0 lst)
          where
            go cnt rest =
                let (x,xs) = (P.head rest, P.tail rest)
                    m = length x
                in case (cnt+m) < n of
                    True -> x:go (cnt+m) xs
                    False -> [take (n-cnt) x]

-- | Take first 'n' bits or whatever is available.
take :: Int -> Bits -> Bits
take n b = case takeMaybe n b of
    Just x -> x
    Nothing -> fromJust $ takeMaybe (length b) b

-- | Drop first 'n' bits (if available)
dropMaybe :: Int -> Bits -> Maybe Bits
dropMaybe 0 b = Just b
dropMaybe n b = do
    guard $ length b >= n
    Just $ case b of
        BitsNoOffset bs ln -> BitsWithOffset bs n (ln-n)
        BitsWithOffset bs offset ln -> BitsWithOffset bs (offset+n) (ln-n)
        BitBools lst -> BitBools $ P.drop n lst
        BitsConcat lst ->
            let (x,xs) = (P.head lst, P.tail lst)
                ln = length x
            in case n <= ln of
                True -> BitsConcat ((drop n x):xs)
                False -> drop (n-ln) (BitsConcat xs)

-- | Drop first 'n' bits.
drop :: Int -> Bits -> Bits
drop n b = case dropMaybe n b of
    Just x -> x
    Nothing -> mempty

-- | Return n'th bit.
testBit :: Bits -> Int -> Bool
testBit (BitsNoOffset bs _) n =
    let (a,b) = divMod n 8
    in B.testBit (BS.index bs a) (7-b)
testBit (BitsWithOffset bs offset _) n =
    let (a,b) = divMod (offset + n) 8
    in B.testBit (BS.index bs a) (7-b)
testBit (BitBools lst) n = lst P.!! n
testBit (BitsConcat lst) n =
    let (a,b) = (head lst, tail lst)
        ln = length a
    in case n < ln of
        True -> testBit a n
        False -> testBit (BitsConcat b) (n-ln)

-- | Is empty bitstring?
null :: Bits -> Bool
null (BitsNoOffset _ n) = n <= 0
null (BitsWithOffset _ _ n) = n <= 0
null (BitBools lst) = P.null lst
null (BitsConcat lst) = all null lst

-- | Return 'n' bits of all zero values.
zeros :: Int -> Bits
zeros n = BitBools $ replicate n False

-- | Generate bitstring of given bit length and value,
-- for example:
--
-- >    fromIntegral 16 (0x1234::Int) == Bits 00010010 00110100
--
fromIntegral :: Integral a => Int -> a -> Bits
fromIntegral n val = BitBools $ reverse $ unfoldr f (n,val)
  where
    f (0,_) = Nothing
    f (n', val') =
        let (a,b) = val' `divMod` 2
        in Just (bitValue b, (n'-1, a))
    bitValue 0 = False
    bitValue _ = True

fromInteger :: Int -> Integer -> Bits
fromInteger = fromIntegral

fromInt :: Int -> Int -> Bits
fromInt = fromIntegral

-- | Check if bitstring is byte aligned.
isAligned :: Bits -> Bool
isAligned b = (length b `mod` 8) == 0

-- | Prepend with zeros such that a value is 8-bit aligned.
makeAligned :: Bits -> Bits
makeAligned b = case n of
    0 -> b
    _ -> BitsConcat [BitBools (replicate n False), b]
  where
    n = (8 - (length b `mod` 8)) `mod` 8

-- | Invert all bits.
complement :: Bits -> Bits
complement = go . shrinkBits
  where
    go = \case
        BitsNoOffset bs ln -> BitsNoOffset (BS.map B.complement bs) ln
        BitsWithOffset bs offset ln -> BitsWithOffset (BS.map B.complement bs) offset ln
        BitBools lst -> BitBools $ fmap not lst
        BitsConcat lst -> BitsConcat $ fmap go lst

-- | Convert bits to a signed number.
toSigned :: Num a => Bits -> a
toSigned b
    | null b = 0
    | (head . unpack $ b) == False = toUnsigned b
    | otherwise = -((toUnsigned $ complement b)+1)

-- | Convert bits to un unsigned number.
toUnsigned :: Num a => Bits -> a
toUnsigned b = foldr f 0 (fmap boolVal $ reverse $ unpack b)
  where
    f x a = a*2 + x

-- | Convert bytestring to Bits.
fromByteString :: BS.ByteString -> Bits
fromByteString bs = BitsNoOffset bs (BS.length bs * 8)

-- | Convert Bits to bytestring.
toByteString :: Bits -> BS.ByteString
toByteString = BS.pack . map packWord8 . break8 . unpack
  where
    break8 :: [a] -> [[a]]
    break8 [] = []
    break8 s = a : break8 b where (a,b) = splitAt 8 s

-- | Helper function to convert from bits to bytesring.
toMaybeByteString :: Bits -> Maybe BS.ByteString
toMaybeByteString x = do
    guard $ isAligned x
    return $ toByteString x

-- | Is any bit set?
anySet :: Bits -> Bool
anySet (BitBools lst) = P.or lst
anySet (BitsConcat lst) = P.any anySet lst
anySet b = P.or $ unpack b

-- | Are all bits set?
allSet :: Bits -> Bool
allSet (BitBools lst) = P.and lst
allSet (BitsConcat lst) = P.all allSet lst
allSet b = P.and $ unpack b

