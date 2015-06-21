
{-
asterix encoder/decoder

Author: Zoran Bosnjak (Sloveniacontrol)

-}

import Data.Monoid

import qualified Data.ByteString as S
import qualified Data.Map as Map

import qualified Bits as B

type Name = String

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
encode (Spare n) = B.toBits $ replicate n False
encode _ = undefined

-- decode items
decode :: Desc -> B.Bits -> Maybe Item

decode Desc {tip=TItem, items=items} b = undefined

decode Desc {tip=TFixed, len=Length1 n} b
    | B.length b > n = Just $ Fixed (B.takeBits n b)
    | B.length b == n = Just $ Fixed b
    | otherwise = Nothing

decode Desc {tip=TSpare, len=Length1 n} b = undefined

main = do

    {-
    rec <- do
        i036 <- 
        i037 <-
        i000 <- 
        return . Compound $ ...
    -}

    return ()
