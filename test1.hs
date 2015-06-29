#!/usr/bin/env runhaskell

import System.Environment (getProgName, getArgs)

import Control.Monad
import Control.Exception (evaluate)
import Control.DeepSeq (force)

import qualified Asterix as A

main = do
    prog <- getProgName
    args <- getArgs

    -- read all files, force evaluation
    s <- sequence $ map readFile args
    allProfiles <- evaluate $ force $ A.getCategoryDescriptions s

    -- (cat, edition, dsc) <- evaluate $ force $ A.getCategoryDescription s
    {- all asterix is evaluated at this point -}

    putStrLn . show . length $ allProfiles
    putStrLn . show . map fst $ allProfiles

