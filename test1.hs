#!/usr/bin/env runhaskell

import System.Environment (getProgName, getArgs)

import Control.DeepSeq (deepseq)

import qualified Asterix as A

main = do
    prog <- getProgName
    args <- getArgs

    s <- readFile . head $ args

    let (cat, edition, dsc) = A.getCategoryDescription s
    deepseq (cat, edition, dsc) (return ())

    putStrLn "--------"
    putStrLn . show $ cat
    putStrLn . show $ edition
    putStrLn . show . A.dName $ dsc
    putStrLn . show . A.dTip $ dsc
    putStrLn . show . A.dDsc $ dsc
    putStrLn . show . A.dLen $ dsc
    putStrLn . show . length . A.dItems $ dsc

