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
        bs = A.Bytes (S.pack [0x02, 0x03])

        i010 = A.decode d010 bs
        i000 = A.decode d000 bs

    putStrLn ""

    putStrLn . show $ d010
    putStrLn . show $ i010

    putStrLn . show $ d000
    putStrLn . show $ i000

