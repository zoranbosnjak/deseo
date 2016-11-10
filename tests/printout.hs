import System.Environment (getProgName, getArgs)

import Data.List
import Data.Monoid

import Data.Maybe

import Control.Monad
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad.State

import qualified Data.ByteString as S

import qualified Data.Asterix as A
import qualified Data.Bits as B

main = do
    prog <- getProgName
    args <- getArgs

    -- read all files, force evaluation
    s <- sequence $ map readFile args
    uaps <- evaluate . force . map (\(cat, (ed, dsc)) -> (cat, dsc)) 
        . A.getCategoryDescriptions [] $ s

    let p018 :: A.Desc
        p018 = fromJust $ A.getUapByName 18 uaps "uap"
        p018_036 = A.getDesc' p018 ["036"]
        p018_036_SAC = A.getDesc' p018 ["036", "SAC"]
        p018_006 = A.getDesc' p018 ["006"]
        p018_008 = A.getDesc' p018 ["008"]
        p048 = fromJust $ A.getUapByName 48 uaps "uap"
        p062 = fromJust $ A.getUapByName 62 uaps "uap"
        p062_105_LAT = A.getDesc' p062 ["105","LAT"]

    let 
        record0 = A.create p048 $ return ()
        
        record1a = A.Item p048 $ B.bits 24 0x800102
        record1b = A.Item p048 $ B.bits 32 0x40010203

        record2 = A.create p048 $ do
            A.setItem "010" $ A.fromValue 0x0102

        record3a = A.create p018 $ do
            A.setItem "036" $ A.fromValue 0x0102

        record3b = A.create p018 $ do
            A.setItem "037" $ A.fromValue 0x0102

        record3c = A.create p018 $ do
            A.setItem "036" $ A.fromValue 0x0102
            A.setItem "037" $ A.fromValue 0x0304

        record3d = A.create p018 $ do
            A.setItem "036" $ A.fromValue 0x0102
            A.setItem "037" $ A.fromValues [("SAC", 0x03), ("SIC", 0x04)]

        record4 = A.create p018 $ do
            A.setItem "006" $ A.fromList 
                [ [("ModeS", 0x010203)]
                , [("ModeS", 0x020304)]
                ]
        
        record5 = A.create p048 $ do 
            A.setItem "130" $ A.fromValues [("SRL", 0xaa), ("SRR", 0xbb)]

        record6 = A.Item p048 $ B.bits 24 0x20FF8E

        item7a = A.Item p018_008 $ B.bits 8 0x00
        item7b = A.Item p018_008 $ B.bits 16 0x0100

        record8 = A.create p062 $ do
            {-
                /105/LAT Fixed (32) 008238ca -> 45.7811129093 deg
                /105/LON Fixed (32) 001d6da2 -> 10.3458702564 deg
                /070 Fixed (24) 2dbacc -> 23413.59375 s
                /290 Compound (32) 70ff1010
                    /290/PSR Fixed (8) ff
                    /290/SSR Fixed (8) 10
                    /290/MDS Fixed (8) 10
            -}
            A.setItem "105" $ A.fromValues
                [("LAT", 0X008238ca), ("LON", 0x001d6da2)]
            A.setItem "070" $ A.fromValue 0x2dbacc
            -- A.setItem "290" $ A.fromValue 0x70ff1010
            -- A.setItem "290" $ A.fromValues 
            --  [("PSR", 0xff), ("SSR", 0x10), ("MDS", 0x10)]

    print $ A.sizeOf p018_036 $ B.bits 100 0
    print $ A.sizeOf p018_036_SAC $ B.bits 100 0
    print $ A.sizeOf p018_036_SAC $ B.bits 1 0
    print p018_006
    print $ A.sizeOf p018_006 $ B.bits 100 0
    print $ A.sizeOf p018_006 $ B.bits 32 0x01000000
    print $ A.sizeOf p018_006 $ B.bits 64 0x0200000000000000
    print $ A.sizeOf p018 $ B.bits 24 0x800102

    print record0
    print $ A.childsComp record0
    print record1a
    print $ A.childsComp record1a
    print record1b
    print $ A.unChildsComp p048 . A.childsComp $ record1b
    print $ A.childsComp record1b
    print record2
    print record3a
    print record3b
    print record3c
    print record3d
    print record4
    print record5
    print $ A.encodeDb $ A.datablock 48 [record5]
    print $ A.childs . fromJust $ return record2 >>= A.child "010"
    print $ return record2 >>= A.child "010" >>= A.child "SAC"
    print $ A.childR ["010","SAC"] record2
    print $ A.toValue . fromJust $ A.childR ["010","SAC"] record2
    print $ A.childs . fromJust $ return record6 >>= A.child "020"
    print $ 
        map A.toValue $ A.childs . fromJust $ return record6 >>= A.child "020"
    print $ A.childs item7a
    print $ A.childs item7b
    print $ p062_105_LAT
    print "---"
    print record8
    print $ return record8 >>= A.child "105" >>= A.child "LAT"
    print $ return record8 >>= A.child "105" >>= A.child "LAT" >>= A.getFloating
    print $ return record8 >>= A.child "105" >>= A.child "LON" >>= A.getFloating
    print $ return record8 >>= A.child "070" >>= A.getFloating
    print $ return record8 >>= A.child "070" >>= A.getRaw
    --print $ return record8 >>= A.child "290"
    --print $ return record8 >>= A.child "290" >>= A.getRaw

    return ()

