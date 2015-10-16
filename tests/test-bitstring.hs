module Main where

import Data.Maybe
import Data.Monoid
import Data.Word
import qualified Data.ByteString as S
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import qualified Data.BitString as B

main = defaultMain tests

tests = [
        testGroup "create" [
                testCase "bits" bits
            ]
        , testGroup "pack" [
                testProperty "pack1" pack1
                , testProperty "pack2" pack2
            ]
        , testGroup "util" [
                testProperty "length" length'
                , testProperty "take" take'
                , testProperty "drop" drop'
                , testProperty "zeros" zeros'
                , testCase "maybe" takeDropMaybe
        ]
        , testGroup "convert" [
                testCase "integral" integral'
                , testProperty "bytestring" bytestring'
        ]
        , testGroup "other" [
                testProperty "combine" combine
        ]
    ]

bits :: Assertion
bits = do
    eq 2 0 [False,False]
    eq 2 1 [False,True]
    eq 2 2 [True,False]
    eq 2 3 [True,True]
    where
        eq n a exp = assertEqual ((show n) ++ "," ++ (show a)) exp (B.unpack $ B.fromIntegral n a)

pack1 :: [Bool] -> Bool
pack1 s = ((B.unpack . B.pack $ s) == s)
         && (B.pack s) == (B.pack . B.unpack . B.pack $ s)

pack2 :: B.Bits -> Bool
pack2 b = (B.pack . B.unpack $ b) == b

length' :: B.Bits -> Bool
length' b = (B.length b) == (length . B.unpack $ b)

take' :: Int -> B.Bits -> Bool
take' n b = (B.take n b) == (B.pack . take n . B.unpack $ b)

drop' :: Int -> B.Bits -> Bool
drop' n b = (B.drop n b) == (B.pack . drop n . B.unpack $ b)

integral' :: Assertion
integral' = do
    assertEqual "to integral" 2 (B.toIntegral . B.pack $ [True,False])
    assertEqual "to integral" 2 (B.toIntegral . B.pack $ [False,True,False])
    assertEqual "to integral" 0 (B.toIntegral . B.pack $ [False,False,False])

bytestring' :: [Word8] -> Bool
bytestring' s = (fromJust . B.toByteString . B.fromByteString . S.pack $ s) == (S.pack s)

combine :: B.Bits -> B.Bits -> Bool
combine a b =   (B.pack (B.unpack a ++ B.unpack b) == c)
                && (B.take (B.length a) c == a)
                && (B.drop (B.length a) c == b)
                where 
                    c = a `mappend` b

zeros' :: Property
zeros' = forAll (choose (0,100)) $ \n ->
            (B.toIntegral . B.zeros $ n) == (0 :: Integer)

takeDropMaybe :: Assertion
takeDropMaybe = do
    let b = B.pack [True, False]
    
    assertEqual "take1" (Just $ B.pack [True]) (B.takeMaybe 1 b)
    assertEqual "take2" (Just $ B.pack [True,False]) (B.takeMaybe 2 b)
    assertEqual "take3" Nothing (B.takeMaybe 3 b)

    assertEqual "drop1" (Just $ B.pack [False]) (B.dropMaybe 1 b)
    assertEqual "drop2" (Just $ B.pack []) (B.dropMaybe 2 b)
    assertEqual "drop3" Nothing (B.dropMaybe 3 b)

