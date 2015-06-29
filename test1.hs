#!/usr/bin/env runhaskell

import System.Environment (getProgName, getArgs)

import Data.List
import Data.Monoid

import Control.Monad
import Control.Exception (evaluate)
import Control.DeepSeq (force)

import qualified Asterix as A

main = do
    prog <- getProgName
    args <- getArgs

    -- read all files, force evaluation
    s <- sequence $ map readFile args
    let requested = [ (2, A.Edition 1 0)
                    ]
    profiles <- evaluate . force . A.getCategoryDescriptions requested $ s

    -- (cat, edition, dsc) <- evaluate $ force $ A.getCategoryDescription s
    {- all asterix is evaluated at this point -}

    forM_ profiles $ \(cat, edition, dsc) -> do
        putStrLn $ show cat
        putStrLn $ "  " ++ show edition

