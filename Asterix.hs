
{-
asterix encoder/decoder

Author: Zoran Bosnjak (Sloveniacontrol)

-}

import Data.Monoid

import qualified Data.ByteString as S
import qualified Data.Map as Map

import qualified Bits as B

import qualified Data.ByteString as S

type Name = String
type Size = Int

type Datagram = B.Bits
type Datablock = B.Bits
type Record = B.Bits

data Tip = TItem
           | TFixed
           | TSpare
           | TExtended
           | TRepetitive
           | TExplicit
           | TCompound
           | TRfs

data Length = Length0 | Length1 Int | Length2 Int Int

data Desc = Desc {  name    :: String
                    , tip   :: Tip
                    , dsc   :: String
                    , len   :: Length
                    , items :: [Desc]

                    -- convert functions
                    , toInt :: Maybe (B.Bits -> Int)
                    , fromInt :: Maybe (Int -> B.Bits)
                    , toFloat :: Maybe (B.Bits -> Float)
                    , fromFloat :: Maybe (Float -> B.Bits)
                 }

noDesc = Desc {
    name = ""
    , tip = TItem
    , dsc = ""
    , len = Length0
    , items = []
    , toInt = Nothing
    , fromInt = Nothing
    , toFloat = Nothing
    , fromFloat = Nothing
}

data Item = Item [(Name,Item)]
            | Fixed B.Bits
            | Spare Int
            | Extended [(Name,Item)]
            | Repetitive [Item]
            | Explicit B.Bits
            | Compound [(Name, Maybe Item)]
            -- | Rfs 

itemLength = B.length . encode

-- encode items
encode :: Item -> B.Bits
encode (Item xs) = mconcat . map (\(_,item) -> encode item) $ xs
encode (Fixed b) = b
encode _ = undefined

-- decode items

checkSize :: Int -> B.Bits -> Maybe Int
checkSize n b
    | B.length b < n = Nothing
    | otherwise = Just n

-- sizeOf
sizeOf :: Desc -> B.Bits -> Maybe Size

-- sizeOf TItem
sizeOf Desc {tip=TItem, len=Length1 n} b = checkSize n b
sizeOf Desc {tip=TItem, items=[]} _ = Just 0
sizeOf d@Desc {tip=TItem, items=(i:is)} b = do
    x <- sizeOf i b
    y <- sizeOf (d {items=is}) (B.drop x b)
    Just (x+y)

-- sizeOf TFixed
sizeOf Desc {tip=TFixed, len=Length1 n} b = checkSize n b

-- sizeOf TSpare
sizeOf Desc {tip=TSpare, len=Length1 n} b = do
    size <- checkSize n b
    if (B.take size b) == (B.zeros size) then Just size
    else Nothing

-- sizeOf TExtended
sizeOf d@Desc {tip=TExtended, len=Length2 n1 n2} b = do
    next <- checkSize n1 b
    if (B.index b (next-1)) then dig next
    else Just n1
        where
            dig offset = do 
                next <- checkSize offset b
                if (B.index b (next-1)) then dig (next+n2)
                else Just next

-- sizeOf TRepetitive
sizeOf Desc {tip=TRepetitive, len=Length1 n} b = do
    next <- checkSize 8 b
    let val = B.toUnsigned . B.take next $ b
    checkSize (8+val*n) b
sizeOf Desc {tip=TRepetitive, items=items} b = undefined

main = do

    let x = B.Bytes $ S.pack [0,0,3,4]
        y = B.Bytes $ S.pack [1,0,3,4]
        d1 = noDesc {tip=TFixed, len=Length1 8}
        d1a = noDesc {tip=TFixed, len=Length1 80}
        d2 = noDesc {tip=TSpare, len=Length1 16}
        d2a = noDesc {tip=TSpare, len=Length1 80}

        d3 = noDesc {tip=TItem, len=Length1 16}
        d3a = noDesc {tip=TItem, items=[d1,d1]}

        d4 = noDesc {tip=TExtended, len=Length2 8 8}

        d5 = noDesc {tip=TRepetitive, len=Length1 24}

    putStrLn . show . sizeOf d1 $ x
    putStrLn . show . sizeOf d1a $ x
    putStrLn . show . sizeOf d2 $ x
    putStrLn . show . sizeOf d2a $ x

    putStrLn "---"
    putStrLn . show . sizeOf d3 $ x
    putStrLn . show . sizeOf d3a $ x

    putStrLn "---"
    putStrLn . show . sizeOf d4 $ x
    putStrLn . show . sizeOf d4 $ y

    putStrLn "---"
    putStrLn . show . sizeOf d5 $ x
    putStrLn . show . sizeOf d5 $ y

    {-
    rec <- do
        i036 <- 
        i037 <-
        i000 <- 
        return . Compound $ ...
    -}

    return ()
