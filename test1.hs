#!/usr/bin/env runhaskell

import System.Environment (getProgName, getArgs)

import Data.List
import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Exception (evaluate)
import Control.DeepSeq (force)

import qualified Data.ByteString as S

import qualified Asterix as A

main = do
    prog <- getProgName
    args <- getArgs

    -- read all files, force evaluation
    s <- sequence $ map readFile args
    let requested = [ (2, A.Edition 1 0)
                    ]
    profiles <- evaluate . force . A.getCategoryDescriptions requested $ s

    forM_ profiles $ \(cat, (edition, dsc)) -> do
        putStrLn $ show cat
        putStrLn $ "  " ++ show edition


    -- get cat002
    let (_, uaps) = fromJust $ lookup 2 profiles
        dsc = A.getUap 2 uaps mempty
        d010 = head . A.dItems $ dsc
        d000 = head . tail . A.dItems $ dsc
        d050 = head . tail . tail . tail . tail . tail . A.dItems $ dsc
        d070 = head . tail . tail . tail . tail . tail . tail . tail . A.dItems $ dsc
        dSP = head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . A.dItems $ dsc
        bs = A.Bytes (S.pack [0x02, 0x03])
        bs2 = A.Bytes (S.pack [0x03, 0x05, 0x02])
        bs3 = A.Bytes (S.pack [0x02, 0x01, 0x02, 0x03, 0x04])
        bs4 = A.Bytes (S.pack [0x40, 0x01, 0x02, 0x03])

        i010 = A.decode d010 bs
        i000 = A.decode d000 bs
        i050 = A.decode d050 bs2
        i070 = A.decode d070 bs3
        iSP  = A.decode dSP bs
        root = A.decode dsc bs4

    putStrLn ""

    putStrLn . show $ d010
    putStrLn . show $ i010

    putStrLn . show $ d000
    putStrLn . show $ i000

    putStrLn . show $ d050
    putStrLn . show $ i050

    putStrLn . show $ d070
    putStrLn . show $ i070

    putStrLn . show $ dSP
    putStrLn . show $ iSP

    putStrLn . show $ dsc
    putStrLn . show $ root

