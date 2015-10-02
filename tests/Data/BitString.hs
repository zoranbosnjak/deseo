
import Data.BitString

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

