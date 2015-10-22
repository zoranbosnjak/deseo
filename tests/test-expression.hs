module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Data.Asterix

main = defaultMain tests

tests = [
        testGroup "Simple" [
                testCase "good" simpleGood
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
    ]

-- helper functions
a === b = eq a b
eq a b = assertEqual ("expression: " ++ (show a)) (Just b) (eval a)
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

