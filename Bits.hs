
{-

Bit string manipulation
(low performance version, based on list)

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Bits
(   Bits
    , bits
    , length
    , take
    , drop
    , index
    , zeros
    , pack
    , unpack
    , null
    , checkAligned
    , toUnsigned
    , fromByteString
    , toByteString
) where

import qualified Prelude as P
import Prelude hiding (length, any, toInteger, take, drop, null, (!!))
import Data.Monoid
import qualified Data.ByteString as S
import Data.Word
import qualified Data.Bits as B
import Data.List (foldl', foldl1')

import Debug.Trace

dump = flip trace

data Bits = Bits [Bool] 
                deriving (Eq)

instance Show Bits where
    show (Bits b) = "Bits " ++ disp b where
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

length (Bits a) = P.length a
take n (Bits a) = Bits $ P.take n a
drop n (Bits a) = Bits $ P.drop n a
index (Bits b) n = (P.!!) b n
pack = Bits
unpack (Bits b) = b
null (Bits b) = P.null b

-- generate bitstring
bits :: Integral a => Int -> a -> Bits
bits n val = Bits . map toBool $ f n val [] where
    f 0 _ acc = acc
    f n val acc = f (n-1) a (b:acc) where
        (a,b) = divMod val 2
    toBool 0 = False
    toBool 1 = True

zeros n = bits n 0

-- is byte aligned
checkAligned :: Bits -> Maybe Bits
checkAligned b = case (length b `mod` 8) of
    0 -> Just b
    _ -> Nothing

-- False=0, True=1
boolVal :: Num a => Bool -> a
boolVal False = 0
boolVal True = 1

-- convert bits to unsigned number
toUnsigned :: Num a => Bits -> a
toUnsigned (Bits b) = sum . zipWith (*) factors . map boolVal . reverse $ b where
    factors = map (2^) [0..]

fromByteString :: S.ByteString -> Bits
fromByteString = Bits . concatMap octet . S.unpack where
    octet w = map (B.testBit w) [7,6..0]

toByteString :: Bits -> Maybe S.ByteString
toByteString bs = do
    (Bits s) <- checkAligned bs
    return . S.pack . map toWord . break8 $ s where
        break8 :: [a] -> [[a]]
        break8 [] = [] 
        break8 s = a : break8 b where (a,b) = splitAt 8 s
        toWord :: [Bool] -> Word8
        toWord s = sum $ zipWith (*) (map (2^) [7,6..0]) (map boolVal s)

main :: IO ()
main = do
    return ()

