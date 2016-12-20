module Main where

import Test.Framework (defaultMain)
import TestAsterix (testAsterix)
import TestBitString (testBitString)
import TestExpression (testExpression)


main :: IO ()
main = defaultMain tests where
    tests = [ testAsterix
            , testBitString
            , testExpression
            ]
