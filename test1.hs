#!/usr/bin/env runhaskell

import System.Environment (getProgName, getArgs)

import Control.Exception (evaluate)
import Control.DeepSeq (deepseq, force)

import qualified Asterix as A

main = do
    prog <- getProgName
    args <- getArgs

    s <- readFile . head $ args
    (cat, edition, dsc) <- evaluate $ force $ A.getCategoryDescription s
    {- all asterix is evaluated at this point -}

    let uap = head dsc

    -- deepseq (cat, edition, dsc) (return ())

    putStrLn "--------"
    putStrLn $ "cat: " ++ (show cat) ++ ", edition: " ++ (show edition)
    putStrLn . show . fst $ uap
    putStrLn . show . A.dName . snd $ uap
    putStrLn . show . A.dTip . snd $ uap
    putStrLn . show . A.dDsc . snd $ uap
    putStrLn . show . A.dLen . snd $ uap
    putStrLn . show . length . A.dItems . snd $ uap

