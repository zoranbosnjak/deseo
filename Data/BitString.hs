----------------
-- |
-- Module       :  Data.BitString
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
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
-- >    >>> B.fromUnsigned 16 0x0102
-- >    Bits 00000001 00000010
-- >    >>> B.toUnsigned $ B.pack [True, False]
-- >    2
--

module Data.BitString (

    Bits

    -- * Bits creation
    , pack

    -- * Util functions
    , length
    , take
    , drop
    , index
    , zeros
    , unpack
    , null
    , checkAligned

    -- * Convert functions
    , fromUnsigned
    , toUnsigned
    , fromByteString
    , toByteString
) where

import Control.Monad
import qualified Prelude as P
import Prelude hiding (length, any, toInteger, take, drop, null, (!!))
import Data.Monoid
import qualified Data.ByteString as S
import Data.Word
import qualified Data.Bits as B
import Data.List (foldl1')

import Test.QuickCheck

-- | Bits data type.
data Bits = Bits [Bool] 
                deriving (Eq)

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

-- | Drop first 'n' bits.
drop :: Int -> Bits -> Bits
drop n (Bits a) = Bits $ P.drop n a

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
zeros n = fromUnsigned n (0::Integer)

-- | Generate bitstring of given bit length and value,
-- for example:
--
-- >    bits 16 0x1234
--
fromUnsigned :: Integral a => Int -> a -> Bits
fromUnsigned n val = Bits . map toBool $ f n val [] where
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

-- | Convert bits to unsigned number.
toUnsigned :: Num a => Bits -> a
toUnsigned (Bits b) = sum . zipWith (*) factors . map boolVal . reverse $ b where
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
        toWord s = sum $ zipWith (*) (map (2^) ([7,6..0]::[Int])) (map boolVal s)

