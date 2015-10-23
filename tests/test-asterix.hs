{-# LANGUAGE CPP #-}

module Main where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.Either

import System.FilePath

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Data.Asterix as A
import qualified Data.BitString as B

xmldir = (</> "xml") $ dropFileName __FILE__

main = defaultMain tests

tests = [
        testGroup "read xml" [
            testCase "good" readGood
            , testCase "bad" readBad
        ], 
        testGroup "datablocks" [
            testCase "decode" dbdecode
            , testCase "encode" dbencode
        ],
        testGroup "records" [
            testCase "decode" splitRec
            , testCase "childs" childs'
        ],
        testGroup "create" [
            testCase "create" testCreate
        ],
        testGroup "convert" [
            testCase "get1" testGet1
            , testCase "get2a" testGet2a
            , testCase "get2b" testGet2b
        ],
        testGroup "util" [
            testCase "sizeof" testSizeOf
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
    cat' <- readFile (xmldir </> "cat000_0.0.xml") >>= return . categoryDescription
    assertRight $ do
        cat <- cat'

        -- TODO: check content

        Right "OK"
    
readBad :: Assertion
readBad = do
    let c = categoryDescription "some invalid xml string"
    assertLeft c

dbdecode :: Assertion
dbdecode = do
    let x0 = B.pack []
        x1 = B.fromXIntegral (8*11) 0x02000bf0931702b847147e
        x2 = B.fromXIntegral (8*10) 0x01000501020200050304

    assertEqual "0 datablocks" (Just []) (toDataBlocks x0)

    assertEqual "1 datablock"
        (Just [DataBlock {dbCat=2, dbData=(B.drop 24 x1)}])
        (toDataBlocks x1)

    assertEqual "2 datablocks"
        (Just [
            DataBlock {dbCat=1, dbData=(B.fromXIntegral 16 0x0102)}
            , DataBlock {dbCat=2, dbData=(B.fromXIntegral 16 0x0304)}
            ])
        (toDataBlocks x2)

dbencode :: Assertion
dbencode = do
    return ()

splitRec :: Assertion
splitRec = do
    cat0 <- readFile (xmldir </> "cat000_0.0.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        parse db = return db 
                >>= toDataBlocks 
                >>= mapM (toRecords profiles)
                >>= return . join
                >>= return . map iBits

        d0 = B.fromXIntegral 32 0x000003
        d1a = B.fromXIntegral 32 0x00000400
        d1b = B.fromXIntegral 48 0x000006800203
        d2 = B.fromXIntegral 72 0x000009800203800405

    assertEqual "0 rec" Nothing (parse d0)
    assertEqual "1a rec" (Just [B.fromXIntegral 8 0]) (parse d1a)
    assertEqual "1b rec" (Just [B.fromXIntegral 24 0x800203]) (parse d1b)
    assertEqual "2 rec" (Just [B.fromXIntegral 24 0x800203, B.fromXIntegral 24 0x800405]) (parse d2)

childs' :: Assertion
childs' = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        parse db = return db 
                >>= toDataBlocks 
                >>= mapM (toRecords profiles)
                >>= return . join

        d = B.fromXIntegral 48 0x000006800203
        Just rr = parse d
        r = head rr
        Just (i010:i020:_) = childs r
        Just i010' = snd i010
        Just (sac:sic:_) = childs i010'

        realSac = B.fromXIntegral 8 0x02
        realSic = B.fromXIntegral 8 0x03

    assertEqual "i010" ("010",True) (fst i010, isJust . snd $ i010)
    assertEqual "i020" ("020",False) (fst i020, isJust . snd $ i020)

    assertEqual "sac" "SAC" (fst sac)
    assertEqual "sic" "SIC" (fst sic)

    assertEqual "sac" realSac (iBits . fromJust . snd $ sac)
    assertEqual "sic" realSic (iBits . fromJust . snd $ sic)

    assertEqual "sac" (Just realSac) (childR ["010", "SAC"] r >>= return . iBits)
    assertEqual "sic" (Just realSic) (childR ["010", "SIC"] r >>= return . iBits)
    assertEqual "sec" Nothing (childR ["010", "SEC"] r >>= return . iBits)

    assertEqual "childs/unchilds" (Just r) (childs r >>= unChilds (iDsc r))

testCreate :: Assertion
testCreate = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        
        rec0 = create cat0'' $ return ()

        rec1a = create cat0'' $ do
                    putItem "010" $ fromBits (B.fromXIntegral 16 0x0102)

        rec1b = create cat0'' $ do
                    putItem "010" $ fromRaw 0x0102

        rec1c = create cat0'' $ do
                    putItem "010" $ fromValues fromRaw [("SAC", 0x01), ("SIC", 0x02)]

        rec1d = create cat0'' $ do
                    "010" <! fromRaw 0x0102

        rec1e = create cat0'' $ do
                    "010" `putItem` fromRaw 0x0102

        rec2 = fromValues fromRaw [("010", 0x0102)] cat0''

    assertEqual "created 0" True (isJust rec0)
    assertEqual "created 1a" True (isJust rec1a)

    assertEqual "not equal" False (rec0==rec1a)
    assertEqual "equal 1b" rec1a rec1b
    assertEqual "equal 1c" rec1a rec1c
    assertEqual "equal 1d" rec1a rec1d
    assertEqual "equal 1e" rec1a rec1e
    assertEqual "equal 2" rec1a rec2

        {-

        rec2 = create cat0 $ do
                    putItem "006" $ fromList [
                                        [("ModeS", 0x010203)]
                                        , [("ModeS", 0x020304)]
        rec3 = create cat0 $ do
                    putItem "007" fromSpare
        -}

testGet1 :: Assertion
testGet1 = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        
        rec = create cat0'' $ do
            "010" <! fromValues fromRaw [("SAC", 0x01), ("SIC", 0x02)]
            "030" <! fromRaw 256
            "031" <! fromValues fromRaw [("X", 0x01), ("Y", 0x02)]

        Just i030 = rec >>= child "030" >>= toNatural

    assertEqual "double" 2.0 i030
    assertEqual "double" (i030 == 2.0) True
    assertEqual "double" (2.0 == i030) True
    assertEqual "double" (i030 > 1.9) True
    assertEqual "double" (1.9 < i030) True
    assertEqual "double" (i030 >= 1.9) True
    assertEqual "double" (i030 < 2.1) True
    assertEqual "double" (i030 <= 2.1) True
    assertEqual "double" (i030 /= 0) True
    assertEqual "double" (i030 /= 0.0) True

testGet2a :: Assertion
testGet2a = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        ae = assertEqual
        
        rec = create cat0'' $ do
            "030" <! fromRaw 256
            "031" <! fromValues fromRaw [("X", 0x01), ("Y", 0x02)]
            "041" <! fromValues fromRaw [("X", 0x01), ("Y", 0x02)]
            "042" <! fromValues fromRaw [("X", 0x01), ("Y", 0x02)]

        Just i030 = rec >>= child "030" >>= toNatural
        Just i031x = rec >>= childR ["031","X"] >>= toNatural
        Just i031y = rec >>= childR ["031","Y"] >>= toNatural
        Just i041x = rec >>= childR ["041","X"] >>= toNatural
        Just i042x = rec >>= childR ["042","X"] >>= toNatural

    ae "030" (EDouble 2) i030
    ae "031x" (EDouble 0.5) i031x
    ae "031y" (EDouble 1.0) i031y
    ae "i041x" (EInteger 1) i041x
    ae "i042x" (EInteger 1) i042x

testGet2b :: Assertion
testGet2b = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        ae = assertEqual
        
        rec = create cat0'' $ do
            "030" <! fromRaw 0xFFFFFF
            "031" <! fromValues fromRaw [("X", 0xFFFFFF), ("Y", 0xFFFFFF)]
            "041" <! fromValues fromRaw [("X", 0xFF), ("Y", 0xFF)]
            "042" <! fromValues fromRaw [("X", 0xFF), ("Y", 0xFF)]

        Just i030 = rec >>= child "030" >>= toNatural
        Just i031x = rec >>= childR ["031","X"] >>= toNatural
        Just i031y = rec >>= childR ["031","Y"] >>= toNatural
        Just i041x = rec >>= childR ["041","X"] >>= toNatural
        Just i042x = rec >>= childR ["042","X"] >>= toNatural

    ae "030" (EDouble (0xffffff/128)) i030
    ae "031x" (EDouble (-0.5)) i031x
    ae "031y" (EDouble (-0.5)) i031y
    ae "i041x" (EInteger (-1)) i041x
    ae "i042x" (EInteger 255) i042x

testSizeOf :: Assertion
testSizeOf = do
    
    cat0 <- readFile (xmldir </> "cat000_1.2.xml") >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

    assertEqual "empty" (Just 0) (sizeOf cat0'' (B.pack []))

