
{-
asterix encoder/decoder

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Asterix
(   Tip(..)
    , Desc(..)
    , sizeOf
    , getCategoryDescription
    , getCategoryDescriptionsAll
    , getCategoryDescriptions
    , Edition(..)
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
sizeOf Desc {dTip=TItem, dLen=Length1 n} b = checkSize n b
sizeOf Desc {dTip=TItem, dItems=[]} _ = Just 0
sizeOf d@Desc {dTip=TItem, dItems=(i:is)} b = do
    x <- sizeOf i b
    y <- sizeOf (d {dItems=is}) (B.drop x b)
    Just (x+y)

-- sizeOf TFixed
sizeOf Desc {dTip=TFixed, dLen=Length1 n} b = checkSize n b

-- sizeOf TSpare
sizeOf Desc {dTip=TSpare, dLen=Length1 n} b = do
    size <- checkSize n b
    if (B.take size b) == (B.zeros size) then Just size
    else Nothing

-- sizeOf TExtended
sizeOf d@Desc {dTip=TExtended, dLen=Length2 n1 n2} b = do
    next <- checkSize n1 b
    if (B.index b (next-1)) then dig next
    else Just n1
        where
            dig offset = do 
                next <- checkSize offset b
                if (B.index b (next-1)) then dig (next+n2)
                else Just next

-- sizeOf TRepetitive
sizeOf Desc {dTip=TRepetitive, dLen=Length1 n} b = do
    next <- checkSize 8 b
    let val = B.toUnsigned . B.take next $ b
    checkSize (8+val*n) b
sizeOf Desc {dTip=TRepetitive, dItems=items} b = undefined

-- sizeOf TExplicit
sizeOf Desc {dTip=TExplicit} b = do
    next <- checkSize 8 b
    let val = B.toUnsigned . B.take next $ b
    case val of
        0 -> Nothing
        otherwise -> checkSize (8*val) b

-- sizeOf TCompound
sizeOf Desc {dTip=TCompound, dItems=items} b' = do
    b <- B.checkAligned b'
    (fspec, fspecTotal) <- getFspec b'
    -- TODO: check length of items (must be >= length of fspec)
    let subitems = map snd . filter (\(flag,item) -> flag) $ zip fspec items
    sub <- dig subitems (B.drop (length fspec) b)
    Just ((length fspecTotal) + sub) where

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

        dig :: [Desc] -> B.Bits -> Maybe Size
        dig [] _ = Just 0
        dig (i:is) b = do
            s <- sizeOf i b
            rest <- dig is (B.drop s b)
            Just (s + rest)

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

getCategoryDescriptions :: [(Category,Edition)] -> [String] -> [(Category, Edition, [(String, Desc)])]
getCategoryDescriptions requested ss = do
    (cat, editions) <- getCategoryDescriptionsAll ss
    let ed = case lookup cat requested of
                Nothing -> last editions
                Just x -> case lookup x editions of
                    Nothing -> error $ "cat: " ++ show cat ++ ", edition: " ++ show x ++ " not found!"
                    Just y -> (x, y)

    return (cat, fst ed, snd ed)

