module TestBitString (
    testBitString
) where

import Data.Word (Word8)
import qualified Data.ByteString as S
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit (Assertion, assertEqual)

import qualified Data.BitString as B

testBitString :: Test
testBitString = testGroup "BitString"
    [
        testGroup "create" [
                testCase "bits" bits
                , testProperty "shrink" shrink'
            ]
        , testGroup "pack" [
                testProperty "pack1" pack1
                , testProperty "pack2" pack2
            ]
        , testGroup "util" [
                testProperty "length" length'
                , testProperty "take" take'
                , testProperty "drop" drop'
                , testProperty "complement" complement'
                , testProperty "testBit" testBit'
                , testProperty "zeros" zeros'
                , testCase "maybe" takeDropMaybe
                , testProperty "anyAll" testAnyAll
        ]
        , testGroup "convert" [
                testCase "integral" integral'
                , testProperty "bytestring" bytestring'
                , testProperty "word8" word8'
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
    eq :: Int -> Integer -> [Bool] -> Assertion
    eq n a s = assertEqual ((show n) ++ "," ++ (show a)) s (B.unpack $
        B.fromInteger n a)

shrink' :: B.Bits -> Bool
shrink' b = B.shrinkBits b == b

pack1 :: [Bool] -> Bool
pack1 s = ((B.unpack . B.pack $ s) == s)
         && (B.pack s) == (B.pack . B.unpack . B.pack $ s)

pack2 :: B.Bits -> Bool
pack2 b = (B.pack . B.unpack $ b) == b

length' :: B.Bits -> Bool
length' b = (B.length b) == (length . B.unpack $ b)

take' :: NonNegative Int -> B.Bits -> Bool
take' (NonNegative n) b
    | B.null b = discard
    | otherwise = (B.take n b) == (B.pack . take n . B.unpack $ b)

drop' :: NonNegative Int -> B.Bits -> Bool
drop' (NonNegative n) b
    | B.null b = discard
    | otherwise = (B.drop n b) == (B.pack . drop n . B.unpack $ b)

complement' :: B.Bits -> Bool
complement' b = check1 && check2 where
    check1 = B.complement (B.complement b) == b
    check2 = case B.null b of
        True -> True
        False -> B.complement b /= b

testBit' :: B.Bits -> NonNegative Int -> Bool
testBit' b (NonNegative n)
    | B.null b = discard
    | otherwise =
        let b1 = B.testBit b m
            b2 = (B.unpack b) !! m
        in b1 == b2
  where
    m = n `mod` (B.length b)

integral' :: Assertion
integral' = do
    let eqi :: String -> Integer -> Integer -> IO ()
        eqi s a b = assertEqual s a b
    eqi "to integral" 1 (B.toSigned . B.pack $ [False,True])
    eqi "to uintegral" 1 (B.toUnsigned . B.pack $ [False,True])
    eqi "to integral" (-2) (B.toSigned . B.pack $ [True,False])
    eqi "to uintegral" 2 (B.toUnsigned . B.pack $ [True,False])
    eqi "to integral" (-1) (B.toSigned . B.pack $ [True,True])
    eqi "to uintegral" 3 (B.toUnsigned . B.pack $ [True,True])
    eqi "to integral" 2 (B.toSigned . B.pack $ [False,True,False])
    eqi "to uintegral" 2 (B.toUnsigned . B.pack $ [False,True,False])
    eqi "to integral" 0 (B.toSigned . B.pack $ [False,False,False])
    eqi "to uintegral" 0 (B.toUnsigned . B.pack $ [False,False,False])

bytestring' :: [Word8] -> Bool
bytestring' s =
    (B.toByteString . B.fromByteString . S.pack $ s)
    == (S.pack s)

word8' :: Word8 -> Bool
word8' x = B.packWord8 (B.unpackWord8 x) == x

combine :: B.Bits -> B.Bits -> Bool
combine a b =
    (B.pack (B.unpack a ++ B.unpack b) == c)
    && (B.take (B.length a) c == a)
    && (B.drop (B.length a) c == b)
  where
    c = a `mappend` b

zeros' :: Property
zeros' = forAll (choose (0,100)) $ \n ->
            (B.toSigned . B.zeros $ n) == (0 :: Integer)

takeDropMaybe :: Assertion
takeDropMaybe = do
    let b = B.pack [True, False]

    assertEqual "take1" (Just $ B.pack [True]) (B.takeMaybe 1 b)
    assertEqual "take2" (Just $ B.pack [True,False]) (B.takeMaybe 2 b)
    assertEqual "take3" Nothing (B.takeMaybe 3 b)

    assertEqual "drop1" (Just $ B.pack [False]) (B.dropMaybe 1 b)
    assertEqual "drop2" (Just $ B.pack []) (B.dropMaybe 2 b)
    assertEqual "drop3" Nothing (B.dropMaybe 3 b)

testAnyAll :: B.Bits -> Bool
testAnyAll b = check1 && check2
  where
    check1 = B.allSet b == and (B.unpack b)
    check2 = B.anySet b == or (B.unpack b)

