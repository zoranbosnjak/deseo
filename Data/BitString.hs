{-

Bit string manipulation
(low performance version, based on list)

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Data.BitString
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
    , main -- run unit tests
) where

import Control.Monad
import qualified Prelude as P
import Prelude hiding (length, any, toInteger, take, drop, null, (!!))
import Data.Monoid
import qualified Data.ByteString as S
import Data.Word
import qualified Data.Bits as B
import Data.List (foldl1')
import Data.Maybe

import Test.QuickCheck

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

length :: Bits -> Int
length (Bits a) = P.length a

take :: Int -> Bits -> Bits
take n (Bits a) = Bits $ P.take n a

drop :: Int -> Bits -> Bits
drop n (Bits a) = Bits $ P.drop n a

index :: Bits -> Int -> Bool
index (Bits b) n = (P.!!) b n

pack :: [Bool] -> Bits
pack = Bits

unpack :: Bits -> [Bool]
unpack (Bits b) = b

null :: Bits -> Bool
null (Bits b) = P.null b

-- generate bitstring
bits :: Integral a => Int -> a -> Bits
bits n val = Bits . map toBool $ f n val [] where
    f 0 _ acc = acc
    f n' val' acc = f (n'-1) a (b:acc) where
        (a,b) = divMod val' 2
    toBool 0 = False
    toBool _ = True

zeros :: Int -> Bits
zeros n = bits n (0::Integer)

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
    factors = map (2^) ([0..]::[Int])

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
        toWord s = sum $ zipWith (*) (map (2^) ([7,6..0]::[Int])) (map boolVal s)

-- run the tests
main :: IO ()
main = do
    putStrLn "quick check test run..."

    quickCheck testPack
    quickCheck testPack2
    quickCheck testByteString
    quickCheck testUnsigned
    quickCheck testCombine
    quickCheck testZeros

    where

    testPack :: [Bool] -> Bool
    testPack s =    ((unpack . pack $ s) == s)
                    && (pack s) == (pack . unpack . pack $ s)

    testPack2 :: Bits -> Bool
    testPack2 b = (pack . unpack $ b) == b

    testByteString :: [Word8] -> Bool
    testByteString s = (fromJust . toByteString . fromByteString . S.pack $ s) == (S.pack s)

    testUnsigned :: NonNegative Int -> Bool
    testUnsigned (NonNegative val) = toUnsigned (bits (n+1) val) == val where
        n = ceiling . logBase (2 :: Double) $ fromIntegral val

    testCombine :: Bits -> Bits -> Bool
    testCombine a b =   (pack (unpack a ++ unpack b) == c)
                        && (take (length a) c == a)
                        && (drop (length a) c == b)
                        where c = a `mappend` b
    
    testZeros :: Property
    testZeros = forAll (choose (0,100)) $ \n -> 
         (toUnsigned . zeros $ n) == (0 :: Integer)

