module TestExpression (
    testExpression
) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Data.Asterix


testExpression :: Test
testExpression = testGroup "Expression"
    [
        testGroup "Simple" [
                testCase "basic" testBasic
                , testCase "good" simpleGood
                , testCase "bad" simpleBad
            ]
        , testGroup "Complex" [
                testCase "good" complexGood
                , testCase "bad" complexBad

        ]
        , testGroup "Functions" [
                testProperty "pow1" testPow1
                , testProperty "pow2" testPow2
        ]
        , testRealisticExpressions
    ]

testBasic :: Assertion
testBasic = do
    assertEqual "itoi" True $ (EInteger 2) > (EInteger 1)
    assertEqual "itoi" True $ (EInteger 2) >= (EInteger 1)
    assertEqual "itoi" True $ (EInteger 2) >= (EInteger 2)
    assertEqual "itoi" True $ (EInteger 2) == (EInteger 2)
    assertEqual "itoi" True $ (EInteger 1) /= (EInteger 2)
    assertEqual "itoi" True $ (EInteger 1) < (EInteger 2)
    assertEqual "itoi" True $ (EInteger 1) <= (EInteger 2)

    assertEqual "itod" True $ (EInteger 2) > (EDouble 1)
    assertEqual "itod" True $ (EInteger 2) >= (EDouble 1)
    assertEqual "itod" True $ (EInteger 2) >= (EDouble 2)
    assertEqual "itod" True $ (EInteger 2) == (EDouble 2)
    assertEqual "itod" True $ (EInteger 1) /= (EDouble 2)
    assertEqual "itod" True $ (EInteger 1) < (EDouble 2)
    assertEqual "itod" True $ (EInteger 1) <= (EDouble 2)

    assertEqual "dtoi" True $ (EDouble 2) > (EInteger 1)
    assertEqual "dtoi" True $ (EDouble 2) >= (EInteger 1)
    assertEqual "dtoi" True $ (EDouble 2) >= (EInteger 2)
    assertEqual "dtoi" True $ (EDouble 2) == (EInteger 2)
    assertEqual "dtoi" True $ (EDouble 1) /= (EInteger 2)
    assertEqual "dtoi" True $ (EDouble 1) < (EInteger 2)
    assertEqual "dtoi" True $ (EDouble 1) <= (EInteger 2)



-- helper functions

(===) :: String -> EValue -> Assertion
a === b = eq a b

eq :: String -> EValue -> Assertion
eq a b = assertEqual ("expression: " ++ (show a)) (Just b) (eval a)

nok :: String -> Assertion
nok e = assertEqual "verify failure" Nothing (eval e)

simpleGood :: Assertion
simpleGood = do
    "1" === 1
    "(1)" === 1
    "1+2" === 3
    "1 + 2 * 3 + 4" === 11
    "-1+2" === 1
    "-5-10" === (-15)
    "2*3" === 6
    "1/2" === 0.5

simpleBad :: Assertion
simpleBad = do
    nok "1x"
    nok "a+b"

complexGood :: Assertion
complexGood = do
    "pow(2,10)" === 1024
    "-180.0/pow(0x02, 23)" === (-2.1457672119140625e-05)

complexBad :: Assertion
complexBad = do
    nok "pow(2,10"
    nok "pow"
    nok "pow()"
    nok "pow(2)"
    nok "-180.0pow(0x02, 23)"

testPow1 :: Integer -> QC.NonNegative Integer -> Bool
testPow1 a (QC.NonNegative b) =
    let s = "pow("++(show a)++","++(show b)++")"
        Just (EInteger val) = eval s
        expected = a^b
    in val==expected

testPow2 :: Integer -> QC.Positive Integer -> Bool
testPow2 a (QC.Positive nb) =
    let b = (-nb)
        s = "pow("++(show a)++","++(show b)++")"
        Just (EDouble val) = eval s
        expected = 1.0 / (fromInteger (a^(-b)))
    in val==expected


testRealisticExpressions :: Test
testRealisticExpressions =
    testGroup "realistic expressions" $ map performTest examples
    where
    performTest (input, expected) =
        testCase ("realistic: " ++ input) $ input === expected
    examples =
        [ ("-100", EInteger (-100))
        , ("-1300", EInteger (-1300))
        , ("-15", EInteger (-15))
        , ("-15.0", EDouble (-15.0))
        , ("-1500", EInteger (-1500))
        , ("-1500.0", EDouble (-1500.0))
        , ("-180", EInteger (-180))
        , ("-180.0", EDouble (-180.0))
        , ("-256.0", EDouble (-256.0))
        , ("-8192.0", EDouble (-8192.0))
        , ("-90", EInteger (-90))
        , ("-90.0", EDouble (-90.0))
        , ("0", EInteger 0)
        , ("0.01", EDouble 1.0e-2)
        , ("0.1", EDouble 0.1)
        , ("0.22", EDouble 0.22)
        , ("0.25", EDouble 0.25)
        , ("0.5", EDouble 0.5)
        , ("1", EInteger 1)
        , ("1.0", EDouble 1.0)
        , ("1.0/128", EDouble 7.8125e-3)
        , ("1.0/256", EDouble 3.90625e-3)
        , ("1.0/32", EDouble 3.125e-2)
        , ("1.0/4", EDouble 0.25)
        , ("1.0/pow(2, -14)", EDouble 16384.0)
        , ("10", EInteger 10)
        , ("100", EInteger 100)
        , ("100000", EInteger 100000)
        , ("127", EInteger 127)
        , ("15", EInteger 15)
        , ("1500", EInteger 1500)
        , ("1500.0", EDouble 1500.0)
        , ("150000", EInteger 150000)
        , ("16", EInteger 16)
        , ("180", EInteger 180)
        , ("180.0", EDouble 180.0)
        , ("180.0/pow(2, 23)", EDouble 2.1457672119140625e-5)
        , ("180.0/pow(2, 25)", EDouble 5.364418029785156e-6)
        , ("180.0/pow(2, 30)", EDouble 1.6763806343078613e-7)
        , ("2", EInteger 2)
        , ("25", EInteger 25)
        , ("25.5", EDouble 25.5)
        , ("256.0", EDouble 256.0)
        , ("300", EInteger 300)
        , ("360", EInteger 360)
        , ("360.0/128", EDouble 2.8125)
        , ("360.0/pow(2, 16)", EDouble 5.4931640625e-3)
        , ("360.0/pow(2,16)", EDouble 5.4931640625e-3)
        , ("512", EInteger 512)
        , ("59", EInteger 59)
        , ("6.25", EDouble 6.25)
        , ("655.35", EDouble 655.35)
        , ("8191.75", EDouble 8191.75)
        , ("90", EInteger 90)
        , ("90.0", EDouble 90.0)
        , ("pow(10,-5)", EDouble 1.0e-5)
        , ("pow(2, -14)", EDouble 6.103515625e-5)
        , ("pow(2, -30)", EDouble 9.313225746154785e-10)
        ]
