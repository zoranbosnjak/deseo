{-# LANGUAGE CPP #-}

module TestAsterix (
    testAsterix
) where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Either (rights)

import System.FilePath ((</>), dropFileName)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, assertFailure, Assertion)

import Data.Asterix as A
import qualified Data.BitString as B

xmldir :: FilePath
xmldir = (</> "xml") $ dropFileName __FILE__

testAsterix :: Test
testAsterix = testGroup "Asterix"
    [
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
            , testCase "childs" childsTest
            , testCase "childs/unchilds" childsUnchildsTest
        ],
        testGroup "create/update" [
            testCase "update" testUpdate
            , testCase "create" testCreate
            , testCase "limits" testLimits
        ],
        testGroup "convert" [
            testCase "get1" testGet1
            , testCase "get2a" testGet2a
            , testCase "get2b" testGet2b
            , testCase "set1" testSet1
        ],
        testGroup "util" [
            testCase "sizeof" testSizeOf
        ],
        testGroup "types" [
            testCase "extended" testExtended
            , testCase "extended variant" testExtendedVariant
            , testCase "repetitive" testRepetitive
            , testCase "string" testString
        ]
    ]

assertLeft :: Show a => Either t a -> IO ()
assertLeft x = case x of
    Left _ -> return ()
    Right val -> assertFailure $ "unexpected value " ++ (show val)

assertRight :: Either String t -> Assertion
assertRight x = case x of
    Left e -> assertFailure e
    _ -> return ()

readGood :: Assertion
readGood = do
    cat <- readFile (xmldir </> "cat000_0.0.xml")
        >>= return . categoryDescription
    assertRight $ do
        _cat' <- cat

        -- TODO: check content

        Right "OK"

readBad :: Assertion
readBad = do
    let c = categoryDescription "some invalid xml string"
    assertLeft c

dbdecode :: Assertion
dbdecode = do
    let x0 = B.pack []
        x1 = B.fromInteger (8*11) 0x02000bf0931702b847147e
        x2 = B.fromInteger (8*10) 0x01000501020200050304

    assertEqual "0 datablocks" (Just []) (toDataBlocks x0)

    assertEqual "1 datablock"
        (Just [DataBlock {dbCat=2, dbData=(B.drop 24 x1)}])
        (toDataBlocks x1)

    assertEqual "2 datablocks"
        (Just [
            DataBlock {dbCat=1, dbData=(B.fromInteger 16 0x0102)}
            , DataBlock {dbCat=2, dbData=(B.fromInteger 16 0x0304)}
            ])
        (toDataBlocks x2)

dbencode :: Assertion
dbencode = do
    return ()

splitRec :: Assertion
splitRec = do
    cat0 <- readFile (xmldir </> "cat000_0.0.xml")
        >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        parse db = return db
                >>= toDataBlocks
                >>= mapM (toRecords profiles)
                >>= return . join
                >>= return . map iBits

        d0 = B.fromInteger 32 0x000003
        d1a = B.fromInteger 32 0x00000400
        d1b = B.fromInteger 48 0x000006800203
        d2 = B.fromInteger 72 0x000009800203800405

    assertEqual "0 rec" Nothing (parse d0)
    assertEqual "1a rec" (Just [B.fromInteger 8 0]) (parse d1a)
    assertEqual "1b rec" (Just [B.fromInteger 24 0x800203]) (parse d1b)
    assertEqual "2 rec"
        (Just [B.fromInteger 24 0x800203, B.fromInteger 24 0x800405])
        (parse d2)

childsTest :: Assertion
childsTest = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        parse db = return db
                >>= toDataBlocks
                >>= mapM (toRecords profiles)
                >>= return . join

        d = B.fromInteger 48 0x000006800203
        Just rr = parse d
        r = head rr
        Just (i010:i020:_) = childs r
        Just i010' = snd i010
        Just (sac:sic:_) = childs i010'

        realSac = B.fromInteger 8 0x02
        realSic = B.fromInteger 8 0x03

    assertEqual "i010" ("010",True) (fst i010, isJust . snd $ i010)
    assertEqual "i020" ("020",False) (fst i020, isJust . snd $ i020)

    assertEqual "sac" "SAC" (fst sac)
    assertEqual "sic" "SIC" (fst sic)

    assertEqual "sac" realSac (iBits . fromJust . snd $ sac)
    assertEqual "sic" realSic (iBits . fromJust . snd $ sic)

    assertEqual "sac"
        (Just realSac)
        (childR ["010", "SAC"] r >>= return . iBits)
    assertEqual "sic"
        (Just realSic)
        (childR ["010", "SIC"] r >>= return . iBits)
    assertEqual "sec" Nothing (childR ["010", "SEC"] r >>= return . iBits)

    assertEqual "childs/unchilds" (Just r) (childs r >>= unChilds (iDsc r))

childsUnchildsTest :: Assertion
childsUnchildsTest = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec = create cat0'' $ do
            "010" <! fromRawInt 0x0102
            "020" <! fromRawInt 0x1234
            "031" <! fromRawInt 1
            "050" <! fromSubitems
                [ ("X", fromRawInt 2)
                , ("Y", fromRawInt 3)
                , ("A", fromRawInt 4)
                , ("B", fromRawInt 5)
                , ("C", fromRawInt 6)
                ]
            "060" <!! do
                "I1" <! fromRawInt 1
                "I3" <! fromRawInt 2
            "070" <! fromRepetitiveValues (fromValues fromRawInt)
                [ [("A", 1), ("B", 2)]
                , [("A", 3), ("B", 4)]
                ]
            "100" <! fromRawInt 9

    -- for each subitem, convert to childs, then back
    forM_ (fromJust $ childs $ fromJust rec) $ \(n, mi) -> case mi of
        Nothing -> return ()
        Just i -> case childs i of
            Nothing -> return ()
            Just j -> do
                let rv = unChilds (iDsc i) j
                assertEqual ("child/unchild " ++ n) mi rv

testUpdate :: Assertion
testUpdate = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec0 = emptyItem cat0''
        Just rec1 = create cat0'' $ do
            putItem "010" $ fromBits (B.fromInteger 16 0x0102)
            putItem "020" $ fromBits (B.fromInteger 16 0xABCD)

    assertEqual "update put"
        (Just rec1)
        (update rec0 $ do
            putItem "010" $ fromBits (B.fromInteger 16 0x0102)
            putItem "020" $ fromBits (B.fromInteger 16 0xABCD)
        )

    assertEqual "update del"
        (Just rec0)
        (update rec1 $ do
            delItem "010"
            delItem "020"
        )

    assertEqual "update modifyItem1a"
        (create cat0'' $ putItem "010" $ fromBits (B.fromInteger 16 0x0103))
        (update rec1 $ do
            modifyItem "010" $ \_ -> fromRawInt 0x0103
            delItem "020"
        )

    assertEqual "update modifyItem1b"
        (update rec1 $ do
            modifyItem "010" $ \_ -> fromRawInt 0x0103
            delItem "020"
        )
        (update rec1 $ do
            modifyItemR ["010"] $ \_ -> fromRawInt 0x0103
            delItem "020"
        )

    assertEqual "update modifyItem2"
        Nothing
        (update rec0 $ modifyItem "010" $ \_ -> fromRawInt 0x0103)

    assertEqual "update modifyItemR1"
        (create cat0'' $ putItem "010" $ fromBits (B.fromInteger 16 0x0103))
        (update rec1 $ do
            modifyItemR ["010", "SIC"] $ \_ -> fromRawInt 0x03
            delItem "020"
        )

    assertEqual "update modifyItemR2"
        Nothing
        (update rec0 $ modifyItemR ["010", "SIC"] $ \_ -> fromRawInt 0x03)

    do
        let Just a = create cat0'' $ do
                putItem "010" $ fromBits (B.fromInteger 16 0x0201)  -- swap SAC/SIC
                putItem "020" $ fromBits (B.fromInteger 16 0xABCD)
            -- method1
            Just b = update rec1 $ do
                let Just sac = childR ["010", "SAC"] rec1 >>= toRaw
                    Just sic = childR ["010", "SIC"] rec1 >>= toRaw
                modifyItemR ["010", "SAC"] $ \_ -> fromRawInt sic
                modifyItemR ["010", "SIC"] $ \_ -> fromRawInt sac
            -- method2
            Just c = update rec1 $ do
                modifyItem "010" $ \i dsc -> do
                    sac <- B.take 8 <$> toBits i
                    sic <- B.take 8 . B.drop 8 <$> toBits i
                    fromBits (sic <> sac) dsc
        assertEqual "swap sac-sic 1" a b
        assertEqual "swap sac-sic 2" a c

testCreate :: Assertion
testCreate = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec0 = create cat0'' $ return ()

        rec1a = create cat0'' $ do
            putItem "010" $ fromBits (B.fromInteger 16 0x0102)

        rec1b = create cat0'' $ do
            putItem "010" $ fromRawInt 0x0102

        rec1c = create cat0'' $ do
            putItem "010" $ fromValues fromRawInt [("SAC", 0x01), ("SIC", 0x02)]

        rec1d = create cat0'' $ do
            "010" <! fromRawInt 0x0102

        rec1e = create cat0'' $ do
            "010" `putItem` fromRawInt 0x0102

        rec2 = fromValues fromRawInt [("010", 0x0102)] cat0''

        rec3 = create cat0'' $ do
            "060" <! fromSubitems
                [ ("I3", fromRawInt 255)
                ]

        rec3a = create cat0'' $ do
            "060" <!! do
                "I3" <! fromRawInt 255

        rec4 = create cat0'' $ do
            "060" <! fromSubitems
                [ ("I1", fromRawInt 254)
                , ("I3", fromRawInt 255)
                ]

        rec4a = create cat0'' $ do
            "060" <!! do
                "I1" <! fromRawInt 254
                "I3" <! fromRawInt 255

        rec5a = create cat0'' $ do
            putItem "010" $ fromRawInt 0x0102

        rec5b = create cat0'' $ do
            putItem "010" $ fromRawInt 0x0102
            putItem "030" $ fromRawInt 256

        rec5c = create cat0'' $ do
            putItem "010" $ fromRawInt 0x0102
            putItem "030" $ fromRawInt 256
            delItem "030"

    assertEqual "created 0" True (isJust rec0)
    assertEqual "created 1a" True (isJust rec1a)

    assertEqual "not equal" False (rec0==rec1a)
    assertEqual "equal 1b" rec1a rec1b
    assertEqual "equal 1c" rec1a rec1c
    assertEqual "equal 1d" rec1a rec1d
    assertEqual "equal 1e" rec1a rec1e
    assertEqual "equal 2" rec1a rec2

    assertEqual "correct rec1a"
        (B.fromInteger 24 0x800102) (iBits $ fromJust rec1a)

    assertEqual "correct rec3"
        (B.fromInteger 32 0x014020FF) (iBits $ fromJust rec3)

    assertEqual "correct rec4"
        (B.fromInteger 40 0x0140A0FEFF) (iBits $ fromJust rec4)

    assertEqual "created rec3" True (isJust rec3)
    assertEqual "equal rec3a" rec3 rec3a

    assertEqual "created rec4" True (isJust rec4)
    assertEqual "equal rec4a" rec4 rec4a

    assertEqual "not equal rec3 rec4" True (rec3 /= rec4)

    assertEqual "delItem1" True (rec5a /= rec5b)
    assertEqual "delItem2" True (rec5a == rec5c)

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
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec = create cat0'' $ do
            "010" <! fromValues fromRawInt [("SAC", 0x01), ("SIC", 0x02)]
            "030" <! fromRawInt 256
            "031" <! fromValues fromRawInt [("X", 0x01), ("Y", 0x02)]

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
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        ae = assertEqual

        rec = create cat0'' $ do
            "030" <! fromRawInt 256
            "031" <! fromValues fromRawInt [("X", 0x01), ("Y", 0x02)]
            "041" <! fromValues fromRawInt [("X", 0x01), ("Y", 0x02)]
            "042" <! fromValues fromRawInt [("X", 0x01), ("Y", 0x02)]

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
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        ae = assertEqual

        rec = create cat0'' $ do
            "030" <! fromRawInt 0xFFFFFF
            "031" <! fromValues fromRawInt [("X", 0xFFFFFF), ("Y", 0xFFFFFF)]
            "041" <! fromValues fromRawInt [("X", 0xFF), ("Y", 0xFF)]
            "042" <! fromValues fromRawInt [("X", 0xFF), ("Y", 0xFF)]

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

testSet1 :: Assertion
testSet1 = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"
        ae = assertEqual

        rec = create cat0'' $ do
            "030" <! fromRawInt (-1)
            "031" <! fromValues fromNatural [("X", 2), ("Y", (-2))]
            "041" <! fromValues fromNatural [("X", (-3)), ("Y", 4)]
            "042" <! fromValues fromNatural [("X", 255), ("Y", 6)]

        Just i030 = rec >>= child "030" >>= toNatural
        Just i031x = rec >>= childR ["031","X"] >>= toNatural
        Just i031y = rec >>= childR ["031","Y"] >>= toNatural
        Just i041x = rec >>= childR ["041","X"] >>= toNatural
        Just i042x = rec >>= childR ["042","X"] >>= toNatural

    ae "030" (EDouble (0xffffff/128)) i030
    ae "031x" (EDouble (2)) i031x
    ae "031y" (EDouble (-2)) i031y
    ae "i041x" (EInteger (-3)) i041x
    ae "i042x" (EInteger 255) i042x

testSizeOf :: Assertion
testSizeOf = do

    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

    assertEqual "empty" (Just 0) (sizeOf cat0'' (B.pack []))

testLimits :: Assertion
testLimits = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec1 = create cat0'' $ do
            "031" <! fromValues fromNatural [("X", 100), ("Y", (-100))]

        rec2a = create cat0'' $ do
            "031" <! fromValues fromNatural [("X", 100.1), ("Y", (-100))]

        rec2b = create cat0'' $ do
            "031" <! fromValues fromNatural [("X", 100), ("Y", (-100.1))]

        rec3a = create cat0'' $ do
            "031" <! fromValues fromRawInt [("X", 200), ("Y", (-500))]

        rec3b = create cat0'' $ do
            "031" <! fromValues fromRawInt [("X", 500), ("Y", (-200))]

    assertEqual "valid"
        True
        (isJust $ rec1 >>= childR ["031","X"] >>= toNatural)
    assertEqual "invalidx"
        True
        (isNothing $ rec2a >>= childR ["031","X"] >>= toNatural)
    assertEqual "invalidy"
        True
        (isNothing $ rec2b >>= childR ["031","X"] >>= toNatural)

    assertEqual "valid" (Just 100) (rec3a >>= childR ["031","X"] >>= toNatural)
    assertEqual "invalidy" Nothing (rec3a >>= childR ["031","Y"] >>= toNatural)
    assertEqual "invalidx" Nothing (rec3b >>= childR ["031","X"] >>= toNatural)
    assertEqual "valid"
        (Just (-100))
        (rec3b >>= childR ["031","Y"] >>= toNatural)

testExtended :: Assertion
testExtended = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec1 = create cat0'' $ "050" <! fromValues fromRawInt [("X", 1)]
        rec2 = create cat0'' $
            "050" <! fromValues fromRawInt [("X", 1),("Y", 2)]
        rec3 = create cat0'' $
            "050" <! fromValues fromRawInt [("X", 1),("Y", 2),("A",3)]
        rec4 = create cat0'' $
            "050" <! fromValues fromRawInt [("X", 1),("Y", 2),("A",3),("B",4)]
        rec5 = create cat0'' $
            "050" <! fromValues fromRawInt
                [("X", 1),("Y", 2),("A",3),("B",4),("C",5)]

        Just c2 = rec2 >>= child "050" >>= childs
        Just c5 = rec5 >>= child "050" >>= childs

    assertEqual "X"     False   (isJust rec1)
    assertEqual "XY"    True    (isJust rec2)
    assertEqual "XYA"   True    (isJust rec3)
    assertEqual "XYAB"  False   (isJust rec4)
    assertEqual "XYABC" True    (isJust rec5)

    assertEqual "len2"  5   (length c2)
    assertEqual "len5"  5   (length c5)

    let aeq :: String -> Maybe Integer -> Maybe Integer -> IO ()
        aeq = assertEqual

    aeq "X" Nothing     (rec1 >>= childR ["050","X"] >>= toRaw)
    aeq "X" (Just 1)    (rec2 >>= childR ["050","X"] >>= toRaw)
    aeq "X" (Just 1)    (rec3 >>= childR ["050","X"] >>= toRaw)
    aeq "X" Nothing     (rec4 >>= childR ["050","X"] >>= toRaw)
    aeq "X" (Just 1)    (rec5 >>= childR ["050","X"] >>= toRaw)

    aeq "Y" Nothing     (rec1 >>= childR ["050","Y"] >>= toRaw)
    aeq "Y" (Just 2)    (rec2 >>= childR ["050","Y"] >>= toRaw)
    aeq "Y" (Just 2)    (rec3 >>= childR ["050","Y"] >>= toRaw)
    aeq "Y" Nothing     (rec4 >>= childR ["050","Y"] >>= toRaw)
    aeq "Y" (Just 2)    (rec5 >>= childR ["050","Y"] >>= toRaw)

    aeq "A" Nothing     (rec1 >>= childR ["050","A"] >>= toRaw)
    aeq "A" Nothing     (rec2 >>= childR ["050","A"] >>= toRaw)
    aeq "A" (Just 3)    (rec3 >>= childR ["050","A"] >>= toRaw)
    aeq "A" Nothing     (rec4 >>= childR ["050","A"] >>= toRaw)
    aeq "A" (Just 3)    (rec5 >>= childR ["050","A"] >>= toRaw)

    aeq "B" Nothing     (rec1 >>= childR ["050","B"] >>= toRaw)
    aeq "B" Nothing     (rec2 >>= childR ["050","B"] >>= toRaw)
    aeq "B" Nothing     (rec3 >>= childR ["050","B"] >>= toRaw)
    aeq "B" Nothing     (rec4 >>= childR ["050","B"] >>= toRaw)
    aeq "B" (Just 4)    (rec5 >>= childR ["050","B"] >>= toRaw)

    aeq "C" Nothing     (rec1 >>= childR ["050","C"] >>= toRaw)
    aeq "C" Nothing     (rec2 >>= childR ["050","C"] >>= toRaw)
    aeq "C" Nothing     (rec3 >>= childR ["050","C"] >>= toRaw)
    aeq "C" Nothing     (rec4 >>= childR ["050","C"] >>= toRaw)
    aeq "C" (Just 5)    (rec5 >>= childR ["050","C"] >>= toRaw)

testExtendedVariant :: Assertion
testExtendedVariant = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec1 = create cat0'' $ "051" <! fromValues fromRawInt [("A", 1)]
        rec2 = create cat0'' $
            "051" <! fromValues fromRawInt [("A", 1),("B", 2)]
        rec3 = create cat0'' $
            "051" <! fromValues fromRawInt [("A", 1),("B", 2),("C",3)]

        Just c2 = rec2 >>= child "051" >>= childs

        aeq :: String -> Maybe Integer -> Maybe Integer -> IO ()
        aeq s a b = assertEqual s a b

    assertEqual "A"     False   (isJust rec1)
    assertEqual "AB"    True    (isJust rec2)
    assertEqual "ABC"   True    (isJust rec3)

    assertEqual "len2"  2   (length c2)

    aeq "A" Nothing     (rec1 >>= childR ["051","A"] >>= toRaw)
    aeq "A" (Just 1)    (rec2 >>= childR ["051","A"] >>= toRaw)
    aeq "A" (Just 1)    (rec3 >>= childR ["051","A"] >>= toRaw)

    aeq "B" Nothing     (rec1 >>= childR ["051","B"] >>= toRaw)
    aeq "B" (Just 2)    (rec2 >>= childR ["051","B"] >>= toRaw)
    aeq "B" (Just 2)    (rec3 >>= childR ["051","B"] >>= toRaw)

    aeq "C" Nothing     (rec1 >>= childR ["051","C"] >>= toRaw)
    aeq "C" Nothing     (rec2 >>= childR ["051","C"] >>= toRaw)
    aeq "C" (Just 3)    (rec3 >>= childR ["051","C"] >>= toRaw)

testRepetitive :: Assertion
testRepetitive = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec1 = create cat0'' $
            "070" <! fromRepetitiveValues (fromValues fromRawInt)
                [ [("A", 1), ("B", 2)]
                , [("A", 3), ("B", 4)]
                ]

        Just c1 = rec1 >>= child "070" >>= childs

    assertEqual "070"   True   (isJust rec1)
    assertEqual "070 childs length"   2 (length c1)

    let result = [(a,decode b) | (a,b) <- c1]
        decode = map decItem . fromJust . childs . fromJust
        decItem (name, i) = (name, B.toUnsigned . iBits $ fromJust i :: Int)
    assertEqual "070 childs"
        [("0", [("A", 1), ("B", 2)]), ("1", [("A", 3), ("B", 4)])]
        result

testString :: Assertion
testString = do
    cat0 <- readFile (xmldir </> "cat000_1.2.xml")
        >>= return . categoryDescription
    let _profiles = Map.fromList [(cCat c, c) | c<-(rights [cat0])]
        (Right cat0') = cat0
        (Just cat0'') = uapByName cat0' "uap"

        rec0 = create cat0'' $ "080" <! fromString ""
        rec1 = create cat0'' $ "080" <! fromString "1"
        rec7 = create cat0'' $ "080" <! fromString "1234567"
        rec8 = create cat0'' $ "080" <! fromString "12345678"

    assertEqual "rec0" Nothing rec0
    assertEqual "rec1" Nothing rec1
    assertEqual "rec7" (Just "1234567") (rec7 >>= child "080" >>= toString)
    assertEqual "rec8" Nothing rec8

