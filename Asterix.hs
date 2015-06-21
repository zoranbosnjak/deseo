
{-
asterix encoder/decoder

Author: Zoran Bosnjak (Sloveniacontrol)

-}

import qualified Data.ByteString as S
import qualified Data.Map as Map

import qualified Bits as B

data Length = Length1 Int | Length2 Int Int

data Convert

data Desc = Desc {  name :: String
                    , dsc   :: Maybe String
                    , len   :: Maybe Length
                    , items :: Maybe [Desc]
                    , convert :: Maybe Convert
                 }

data Item = Raw B.Bits
            | Item Desc [Item]
            | Fixed Desc B.Bits
            | Spare Desc
            | Extended Desc [Item]
            | Repetitive Desc [Item]
            | Explicit Desc
            | Compound Desc [Item]
            -- | Rfs 

encode :: Item -> B.Bits
encode = undefined
    
itemLength = B.length . encode

main = do

    {-
    rec <- do
        i036 <- 
        i037 <-
        i000 <- 
        return . Compound $ ...
    -}

    return ()
