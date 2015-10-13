{-# LANGUAGE CPP #-}

module Main where

import Data.Either

import System.FilePath

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import qualified Data.Asterix as A

xmldir = (</> "xml") $ dropFileName __FILE__

main = defaultMain tests

tests = [
        testGroup "read xml" [
                testCase "good" readGood
                , testCase "bad" readBad
            ]
    ]

assertLeft x = case x of
    Left e -> return ()
    Right val -> assertFailure $ "unexpected value " ++ (show val)

assertRight x = case x of
    Left e -> assertFailure e
    _ -> return ()

readGood :: Assertion
readGood = do
    cat' <- readFile (xmldir </> "cat000_0.0.xml") >>= return . A.categoryDescription
    assertRight $ do
        cat <- cat'

        -- TODO: check content

        Right "OK"
    
readBad :: Assertion
readBad = do
    let c = A.categoryDescription "some invalid xml string"
    assertLeft c

