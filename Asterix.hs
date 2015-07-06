
{-
asterix encoder/decoder

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Asterix
(   Tip(..)
    , Item(..)
    , Desc(..)
    , getCategoryDescription
    , getCategoryDescriptionsAll
    , getCategoryDescriptions
    , Edition(..)
    , getUap
    , B.Bits(..)
    , decode
    , encode 
    , DataBlock(..)
    , toDataBlocks
    , toRecords
    , subitem
) where

import Data.List
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid

import Control.DeepSeq.Generics
import Control.Exception

import qualified Data.ByteString as S
import qualified Data.Map as Map

import Text.XML.Light

import qualified Bits as B

import Debug.Trace

debug = flip trace

type Category = Int
type Name = String
type Size = Int

data Edition = Edition Int Int

instance NFData Edition

instance Eq Edition where
    Edition a1 b1 == Edition a2 b2 = (a1==a2) && (b1==b2)

instance Ord Edition where
    compare (Edition a1 b1) (Edition a2 b2) = (compare a1 a2) `mappend` (compare b1 b2)

instance Show Edition where
    show (Edition a b) = show a ++ "." ++ show b

instance Read Edition where
    readsPrec _ value = [(Edition (read a) (read b), "")] where
        a = takeWhile (/='.') value
        b = tail . dropWhile (/='.') $ value

data DataBlock = DataBlock Category B.Bits
instance Show DataBlock where
    show (DataBlock cat b) = "DataBlock ("++(show cat)++") len: "++(show . B.length $ b)

toDataBlocks :: B.Bits -> [DataBlock]
toDataBlocks bs =   if (B.null bs) then []
                    else db:(toDataBlocks bs') where
                        x = fromJust $ B.checkAligned bs
                        cat = B.toUnsigned $ B.take 8 x
                        len = B.toUnsigned . B.take 16 . B.drop 8 $ x
                        y = B.drop 24 . B.take (len*8) $ x
                        db = DataBlock cat y
                        bs' = B.drop (len*8) x

data Tip = TItem
           | TFixed
           | TSpare
           | TExtended
           | TRepetitive
           | TExplicit
           | TCompound
           -- | TRfs
           deriving (Show, Read)

data Length = Length0 | Length1 Int | Length2 Int Int deriving (Show, Read)

data Desc = Desc {  dName       :: String
                    , dTip      :: Tip
                    , dDsc      :: String
                    , dLen      :: Length
                    , dItems    :: [Desc]

                    -- convert functions
                    , dToInt    :: Maybe (B.Bits -> Int)
                    , dFromInt  :: Maybe (Int -> B.Bits)
                    , dToFloat  :: Maybe (B.Bits -> Float)
                    , dFromFloat :: Maybe (Float -> B.Bits)
                 }

instance NFData Desc

instance Show Desc where
    show d = (show . dTip $ d) ++ " (" ++ (dName d) ++ "), " ++ (show . dLen $ d)

noDesc = Desc {
    dName = ""
    , dTip = TItem
    , dDsc = ""
    , dLen = Length0
    , dItems = []
    , dToInt = Nothing
    , dFromInt = Nothing
    , dToFloat = Nothing
    , dFromFloat = Nothing
}

data Item = Item [Item]
            | Fixed B.Bits
            | Spare Int
            | Extended ItemData
            | Repetitive [Item]
            | Explicit B.Bits
            | Compound [Maybe Item]
            -- | Rfs 
            deriving (Show)

data ItemData = Decoded [Item]
                | Raw B.Bits
                deriving (Show)

itemLength = B.length . encode

-- encode items
encode :: Item -> B.Bits
encode (Item xs) = mconcat . map encode $ xs
encode (Fixed b) = b

-- decode items
decode :: Desc -> B.Bits -> Maybe (Item, Size)

-- decode Item (decode first subitem, decode the rest, then combine)
decode Desc {dTip=TItem, dItems=[]} _ = Just (Item [], 0)
decode d@Desc {dTip=TItem, dItems=(i:is)} b = do
    (x,s1) <- decode i b
    (Item y,s2) <- decode (d {dItems=is}) (B.drop s1 b)
    let size = case (dLen d) of
                -- if the size is known, take it
                (Length1 n) -> n
                otherwise -> s1+s2
    Just (Item (x:y), size)

-- decode Fixed
decode Desc {dTip=TFixed, dLen=Length1 n} b = do
    size <- checkSize n b
    Just (Fixed $ B.take size b, size)

-- decode Spare
decode Desc {dTip=TSpare, dLen=Length1 n} b = do
    size <- checkSize n b
    if (B.take size b) /= (B.zeros size) then Nothing
    else Just (Spare size, size)

-- decode Extended
decode d@Desc {dTip=TExtended, dLen=Length2 n1 n2} b = do
    size <- checkSize n1 b
    if (B.index b (size-1)) then dig size
    else Just (fetch size, size)
        where
            fetch size = Extended . Raw . B.take size $ b
            dig offset = do 
                size <- checkSize offset b
                if (B.index b (size-1)) then dig (size+n2)
                else Just (fetch size, size)

-- decode Repetitive
decode d@Desc {dTip=TRepetitive} b = do
    s8 <- checkSize 8 b
    let rep = B.toUnsigned . B.take s8 $ b
        b' = B.drop s8 b
    (items, size) <- decodeSubitems rep b' [] 8
    Just (Repetitive (reverse items), calcSize size rep) where
        calcSize size rep = case (dLen d) of
            Length1 a -> (8+rep*a)
            otherwise -> size
        decodeSubitems :: Int -> B.Bits -> [Item] -> Size -> Maybe ([Item], Size)
        decodeSubitems 0 _ acc size = Just (acc, size)
        decodeSubitems n b acc size = do
            -- decode single group of items as TItem
            (item, itemSize) <- decode (d {dTip=TItem}) b
            decodeSubitems (n-1) (B.drop itemSize b) (item:acc) (itemSize+size)

-- decode Explicit
decode Desc {dTip=TExplicit} b = do
    s8 <- checkSize 8 b
    let val = B.toUnsigned . B.take s8 $ b
    case val of
        0 -> Nothing
        otherwise -> do
            size <- checkSize (8*val) b
            Just (Explicit . B.take size $ b, size)

-- decode Compound
decode Desc {dTip=TCompound, dItems=items} b' = do
    b <- B.checkAligned b'
    (fspec, fspecTotal) <- getFspec b
    -- TODO: check length of items (must be >= length of fspec)
    let subitems :: [Maybe Desc]
        subitems = map (\(f,i) -> if f then Just i else Nothing) $ zip fspec items
        offset = length fspecTotal

    (items, size) <- dig subitems (B.drop offset b) [] offset
    Just (Compound (reverse items), size) where

        dig :: [Maybe Desc] -> B.Bits -> [Maybe Item] -> Size -> Maybe ([Maybe Item], Size)
        dig [] _ acc size = Just (acc, size)
        dig (x:xs) b acc size = do
            (item, itemSize) <- decodeItem x b
            dig xs (B.drop itemSize b) (item:acc) (itemSize+size)
                where
                    decodeItem Nothing _ = return (Nothing, 0)
                    decodeItem (Just dsc) b = do
                        (item, size) <- decode dsc b
                        return (Just item, size)

        getFspec :: B.Bits -> Maybe ([Bool],[Bool])
        getFspec b = do
            n <- checkSize 8 b
            let val = B.unpack . B.take n $ b
            if (last val == False) then Just ((init val), val)
            else do
                (rem, remTotal) <- getFspec (B.drop n b)
                let rv = (init val) ++ rem
                    rvTotal = val ++ remTotal
                Just (rv, rvTotal)

-- check that requested number of bits are available
checkSize :: Int -> B.Bits -> Maybe Int
checkSize n b
    | B.length b < n = Nothing
    | otherwise = Just n

-- read xml (may fail in case of errors in xml)
getCategoryDescription :: String -> (Category, Edition, [(String, Desc)])
getCategoryDescription s = (cat, ed, dsc) where
    name s = blank_name {qName=s}
    elements = onlyElems . parseXML $ s
    category = head . filter (\e -> (qName . elName $ e) == "category") $ elements
    cat = getAttr category "cat"
    ed = getAttr category "edition"
    items = map (\i -> (dName (readItem i), readItem i)) . elChildren . fromJust . getChild category $ "items"
    dsc = do
        uap <- elChildren . fromJust . getChild category $ "uaps"
        let uapName = qName . elName $ uap
            uapItems = do 
                item <- map strContent . elChildren $ uap
                return $ case item of
                    "" -> noDesc
                    otherwise -> fromJust $ lookup item items
            topLevel = Desc {
                dName = ""
                , dTip = TCompound
                , dDsc = "Category " ++ (show cat)
                , dLen = Length0
                , dItems = force uapItems
                , dToInt = Nothing
                , dFromInt = Nothing
                , dToFloat = Nothing
                , dFromFloat = Nothing
            }
        return (uapName, topLevel)

    getAttr el aName = read . fromJust . findAttr (name aName) $ el
    getChild el aName = findChild (name aName) $ el

    readLength :: String -> Length
    readLength s
        | s == "" = Length0
        | isJust (maybeRead s :: Maybe Size) = Length1 . read $ s
        | isJust (maybeRead s :: Maybe (Size,Size)) = Length2 a b
        where
            (a,b) = read s
            maybeRead s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

    recalculateLen :: Desc -> Length
    recalculateLen dsc = fromMaybe Length0 (total dsc >>= Just . Length1) where
        total :: Desc -> Maybe Size
        total Desc {dLen=Length1 a} = Just a
        total Desc {dLen=Length2 a b} = Nothing
        total Desc {dItems=[]} = Just 0
        total dsc@Desc {dItems=(i:is)} = do
            x <- total i
            rest <- total (dsc {dItems=is})
            Just (x + rest)
    
    readItem :: Element -> Desc
    readItem e = f dsc where

        -- check description, recalculate length
        f :: Desc -> Desc

        f dsc@Desc {dTip=TItem, dLen=Length0, dItems=items@(i:is)} =
            dsc {dLen=recalculateLen dsc}
        f dsc@Desc {dTip=TFixed, dLen=Length1 _, dItems=[]} = dsc
        f dsc@Desc {dTip=TSpare, dLen=Length1 _, dItems=[]} = dsc
        f dsc@Desc {dTip=TExtended, dLen=Length2 _ _} = dsc
        f dsc@Desc {dTip=TRepetitive, dLen=Length0, dItems=items@(i:is)} = 
            dsc {dLen=recalculateLen dsc}
        f dsc@Desc {dTip=TExplicit, dLen=Length0, dItems=[]} = dsc
        f dsc@Desc {dTip=TCompound, dLen=Length0, dItems=items@(i:is)} = dsc
        f x = error $ "error in description: " ++ (dName x)

        -- get all elements
        dsc = Desc {
            dName = fromJust . findAttr (name "name") $ e
            , dTip = read . ("T"++) . fromMaybe "Item" . findAttr (name "type") $ e
            , dDsc = fromMaybe "" (getChild e "dsc" >>= return . strContent)
            , dLen = readLength $ fromMaybe "" (getChild e "len" >>= return . strContent)
            , dItems = map readItem . fromMaybe [] $ do
                getChild e "items" >>= return . elChildren
            , dToInt = Nothing
            , dFromInt = Nothing
            , dToFloat = Nothing
            , dFromFloat = Nothing
        }

getCategoryDescriptionsAll :: [String] -> [(Category, [(Edition, [(String, Desc)])])]
getCategoryDescriptionsAll ss = [(cat, editions cat) | cat <- cats] where
    all = sortBy cmp . map getCategoryDescription $ ss
    cats = nub . map (\(c,_,_) -> c) $ all
    editions cat = noDups cat . map (\(_,e,d) -> (e,d)) . filter (\(c,_,_) -> (c==cat)) $ all
    cmp (c1,e1,_) (c2,e2,_) = (compare c1 c2) `mappend` (compare e1 e2)
    noDups cat editions 
        | nub ed == ed = editions
        | otherwise = error $ "duplicated editions in cat: " ++ show cat
        where
            ed = map fst editions

getCategoryDescriptions :: [(Category,Edition)] -> [String] -> [(Category, (Edition, [(String, Desc)]))]
getCategoryDescriptions requested ss = do
    (cat, editions) <- getCategoryDescriptionsAll ss
    let ed = case lookup cat requested of
                Nothing -> last editions
                Just x -> case lookup x editions of
                    Nothing -> error $ "cat: " ++ show cat ++ ", edition: " ++ show x ++ " not found!"
                    Just y -> (x, y)

    return (cat, (fst ed, snd ed))

getUap :: Category -> [(Category, [(String, Desc)])] -> B.Bits -> Maybe Desc
getUap 1 uaps b = undefined
getUap cat uaps _ = lookup cat uaps >>= lookup "uap"

-- split datablock to records
toRecords :: [(Category, [(String, Desc)])] -> DataBlock -> Maybe [(Item, Desc)]
toRecords profiles (DataBlock cat bs) = getRecord profiles cat bs [] where
    getRecord profiles cat bs acc = 
        if (B.null bs) then Just . reverse $ acc
        else do
            dsc <- getUap cat profiles bs
            (item,size) <- decode dsc bs
            getRecord profiles cat (B.drop size bs) ((item,dsc):acc)

subitem :: [String] -> (Item, Desc) -> Maybe (Item, Desc)
subitem [] (item, dsc) = Just (item,dsc)
subitem (x:xs) (item, dsc) = do
    ix <- elemIndex x (map dName (dItems dsc))
    item2 <- getSubItem item ix
    let dsc2 = (dItems dsc) !! ix
    subitem xs (item2, dsc2) where
        getSubItem (Item items) ix = Just (items !! ix)
        getSubItem (Compound maybeItems) ix = maybeItems !! ix
    
