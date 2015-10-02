
{-
asterix encoder/decoder

Author: Zoran Bosnjak (Sloveniacontrol)

-}

module Data.Asterix
(   Tip(..)
    , Item(..)
    , Desc(..)
    , getCategoryDescription
    , getCategoryDescriptionsAll
    , getCategoryDescriptions
    , Edition(..)
    , getUapByName
    , getUapByData
    , encode 
    , encodeDb
    , datablock
    , DataBlock
    , dbCat
    , toDataBlocks
    , toRecords
    , getDesc
    , getDesc'
    , create
    , fromRaw
    , fromValue
    , fromValues
    , fromList
    , toValue
    , toName
    , setItem
    , sizeOf
    , child
    , childs
    , childR
    , childsComp
    , unChildsComp
    , getFspec

    -- converters
    , getRaw
    , getString
    , getFloating
    , getInteger
    , setRaw
    , setString
    , setFloating
    , setInteger

    ,main
) where

import Data.List
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid

import Control.DeepSeq.Generics
import Control.Exception
import Control.Monad.State

import qualified Data.ByteString as S
import qualified Data.Map as Map

import qualified Text.XML.Light as X

import qualified Data.BitString as B
import Data.Asterix.Expression

type Category = Int
type Name = String
type Size = Int
type Major = Int
type Minor = Int

data Edition = Edition Major Minor

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

dbCat :: DataBlock -> Category
dbCat (DataBlock c _) = c

-- compose datablock
datablock :: Category -> [Item] -> DataBlock
datablock cat items = DataBlock cat bs where
    bs = c `mappend` ln `mappend` records
    c = B.bits 8 cat
    ln = B.bits 16 ((B.length records `div` 8) + 3)
    records = mconcat $ map encode items

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
           deriving (Show, Read, Eq)

data Length = Length0 | Length1 Int | Length2 Int Int deriving (Show, Read, Eq)

type Lsb = Double
type Unit = String
type Min = Double
type Max = Double

data Value = 
    VRaw
    | VString
    | VDecimal Lsb (Maybe Unit) (Maybe Min) (Maybe Max)
    | VUnsignedDecimal Lsb (Maybe Unit) (Maybe Min) (Maybe Max)
    | VInteger (Maybe Unit) (Maybe Min) (Maybe Max)
    | VUnsignedInteger (Maybe Unit) (Maybe Min) (Maybe Max)
    deriving (Eq, Show)

data Desc = Desc {  dName       :: String
                    , dTip      :: Tip
                    , dDsc      :: String
                    , dLen      :: Length
                    , dItems    :: [Desc]
                    , dValue    :: Value
                 } deriving (Eq)

-- convert functions: Item -> Maybe natural value
getRaw :: Item -> Maybe B.Bits
getRaw = Just . encode

getString :: Item -> Maybe String
getString = undefined

getFloating item = do
    let raw lsb b = lsb * B.toUnsigned b
        chk Nothing _ val = Just val
        chk (Just limit) cmp val
            | val `cmp` limit = Nothing
            | otherwise = Just val
        plus val
            | val >= 0 = Just val
            | otherwise = Nothing
    case item of
        Item (d@Desc {dValue=VDecimal lsb unit mmin mmax}) b -> do
            return (raw lsb b) >>= chk mmin (<) >>= chk mmax (>)
        Item (d@Desc {dValue=VUnsignedDecimal lsb unit mmin mmax}) b -> do
            return (raw lsb b) >>= plus >>= chk mmin (<) >>= chk mmax (>)
        _ -> Nothing

getInteger :: Integral a => Item -> Maybe a
getInteger = undefined

-- convert functions: natural value -> Maybe Item
setRaw :: B.Bits -> Maybe Item
setRaw = undefined

setString :: String -> Maybe Item
setString = undefined

setFloating :: Real a => a -> Maybe Item
setFloating = undefined

setInteger :: Integral a => a -> Maybe Item
setInteger = undefined

instance NFData Desc

instance Show Desc where
    show d = (show . dTip $ d) ++ " (" ++ (dName d) ++ "), " ++ (show . dLen $ d) ++ ", " ++ (show $ dValue d)

noDesc = Desc {
    dName = ""
    , dTip = TItem
    , dDsc = ""
    , dLen = Length0
    , dItems = []
    , dValue = VRaw
}

data Item = Item Desc B.Bits deriving (Show,Eq)

-- create item helper functions
-- f :: Content -> Desc -> Item
-- if a content is applied to the function, the return
-- value is a general transform function from Desc -> Item

fromRaw :: B.Bits -> Desc -> Item
fromRaw val d@Desc {dTip=TExplicit} = assert (ln <= 255) (Item d encode) where
    ln = assert (b==0) (a+1)
    (a,b) = divMod (B.length val) 8
    encode = (B.bits 8 ln) `mappend` val

fromValue :: Integral a => a -> Desc -> Item
fromValue val d@Desc {dLen=Length1 len} = case (dTip d) of
    TItem -> Item d (B.bits len val)
    TFixed -> Item d (B.bits len val)
fromValue val d@Desc {dTip=TCompound} = undefined
    -- TODO: take value, assume length is 8-bit aligned value, decode, encode, check

fromValues :: Integral a => [(String,a)] -> Desc -> Item
fromValues values d@Desc {dTip=TItem, dItems=items} = assert (length values == length items) (combine items) where
    l = [assert (dName d'==name) (fromValue val d') | ((name,val),d') <- zip values items]
    combine items = Item d $ mconcat . map encode $ l
fromValues values@(v:vs) d@Desc {dTip=TCompound, dItems=items} = create d transform where
    transforms = map (\(name,val) -> setItem name $ fromValue val) $ values
    transform = foldr (>>) noChange transforms
    noChange = state $ \i -> ((), i)
fromValues values d = undefined -- TODO define function for other types

fromList :: Integral a => [[(String,a)]] -> Desc -> Item
fromList values d@Desc {dTip=TRepetitive, dItems=items} = Item d (ln `mappend` (combine items)) where
    ln = assert (length values < 256) (B.bits 8 $ length values)
    combine items = mconcat . map encode . map (\val -> fromValues val (d {dTip=TItem})) $ values

fromSpare :: Desc -> Item
fromSpare d@Desc {dTip=TSpare, dLen=Length1 len} = Item d (B.bits len 0)

toValue :: Integral a => Item -> a
toValue item = B.toUnsigned . encode $ item

toName :: Item -> String
toName (Item d _) = dName d

-- create record from profile and transform function
create :: Desc -> State Item () -> Item
create profile@Desc {dTip=TCompound} transform = execState transform $ emptyRecord profile where
    emptyRecord d = Item d $ B.bits 0 0

-- set subitem to compound item (stateful computation)
setItem :: String -> (Desc -> Item) -> State Item ()
setItem name toItem = state $ \i@(Item dsc@Desc {dTip=TCompound} bs) -> ((),newItem dsc i) where
    newItem parent i = unChildsComp parent . replace name toItem parent . childsComp $ i
    replace name toItem parent items = map (\(a,b) -> if a==name then (a, Just item) else (a,b)) items where
        dsc = fromJust $ getDesc parent [name] 
        item = toItem dsc

delItem :: String -> State Item ()
delItem = undefined

-- calculate items size
sizeOf :: Desc -> B.Bits -> Maybe Size

-- size of Item
sizeOf d@Desc {dTip=TItem, dLen=Length1 n} b = checkSize n b
sizeOf d@Desc {dTip=TItem, dItems=[]} _ = Just 0
sizeOf d@Desc {dTip=TItem, dItems=(i:is)} b = do
    size <- sizeOf i b
    rest <- sizeOf (d {dItems=is}) (B.drop size b)
    Just (size+rest)

-- size of Fixed
sizeOf d@Desc {dTip=TFixed, dLen=Length1 n} b = checkSize n b

-- size of Spare
sizeOf d@Desc {dTip=TSpare, dLen=Length1 n} b = do
    size <- checkSize n b
    if (B.take size b) /= (B.zeros size) 
        then Nothing
        else Just size

-- size of Extended
sizeOf d@Desc {dTip=TExtended, dLen=Length2 n1 n2} b = do
    size <- checkSize n1 b
    if (B.index b (size-1)) 
        then dig size
        else Just size
    where
        dig offset = do 
            size <- checkSize offset b
            if (B.index b (size-1)) 
                then dig (size+n2)
                else Just size

-- size of Repetitive
sizeOf d@Desc {dTip=TRepetitive} b = do
    s8 <- checkSize 8 b
    let rep = B.toUnsigned . B.take s8 $ b
        b' = B.drop s8 b
    getSubitems rep b' 8
    where
        getSubitems :: Int -> B.Bits -> Size -> Maybe Size
        getSubitems 0 _ size = Just size
        getSubitems n b size = do
            itemSize <- sizeOf (d {dTip=TItem}) b
            getSubitems (n-1) (B.drop itemSize b) (itemSize+size)

-- size of Explicit
sizeOf d@Desc {dTip=TExplicit} b = do
    s8 <- checkSize 8 b
    let val = B.toUnsigned . B.take s8 $ b
    case val of
        0 -> Nothing
        otherwise -> checkSize (8*val) b

-- size of Compound
sizeOf d@Desc {dTip=TCompound, dItems=items} b' = do
    b <- B.checkAligned b'
    (fspec, fspecTotal) <- getFspec b
    -- TODO: check length of items (must be >= length of fspec)
    let subitems :: [(String,Desc)]
        subitems = [(dName dsc,dsc) | (f,dsc) <- zip fspec items, (f==True)]
        offset = length fspecTotal

    dig subitems (B.drop offset b) offset
    where

        dig :: [(String,Desc)] -> B.Bits -> Size -> Maybe Size
        dig [] _ size = Just size
        dig (x:xs) b size = do
            itemSize <- sizeOf (snd x) b
            dig xs (B.drop itemSize b) (itemSize+size)

getFspec :: B.Bits -> Maybe ([Bool],[Bool])
getFspec b = do
    n <- checkSize 8 b
    let val = B.unpack . B.take n $ b
    if (last val == False) 
        then Just ((init val), val)
        else do
            (rem, remTotal) <- getFspec (B.drop n b)
            let rv = (init val) ++ rem
                rvTotal = val ++ remTotal
            Just (rv, rvTotal)

-- get subitem
child :: Name -> Item -> Maybe Item
child name i@(Item d@Desc {dTip=TCompound} b) = fromMaybe Nothing . lookup name $ childsComp i
child name i@(Item d@Desc {dTip=TItem, dItems=items} b) = lookup name $ zip (map dName items) (childs i)

-- get deep subitem, like ["010", "SAC"]
childR :: [Name] -> Item -> Maybe Item
childR [] item = Just item
childR (i:is) item = do
    c <- child i item
    childR is c

-- get items
childs :: Item -> [Item]
childs (Item d@Desc {dTip=TItem, dItems=items} b) = collect items b [] where
    collect [] _ acc = reverse acc
    collect (i:is) b acc = collect is (B.drop size b) $ (Item i (B.take size b)):acc where
        (Just size) = sizeOf i b

-- get childs of extended item
childs (Item d@Desc {dTip=TExtended, dLen=Length2 n1 n2, dItems=items} b) = collect items b [] chunks where
    chunks = [n1] ++ repeat n2

    collect items b acc chunks
        | B.null b = acc
        | otherwise = collect items' b2 acc' chunks'
        where
            (b1, b2) = (B.take (n-1) b, B.drop n b)
            (n, chunks') = (head chunks, tail chunks)
            (items',rv) = take b1 items []
            acc' = acc ++ rv
            take b items acc
                | B.null b = (items,reverse acc)
                | otherwise = take (B.drop size b) (tail items) (item:acc)
                where
                    i = head items
                    item = Item i (B.take size b)
                    (Just size) = sizeOf i b

-- get childs of repetitive item
childs (Item Desc {dTip=TRepetitive} _) = undefined

-- get compound subitems
childsComp :: Item -> [(Name,Maybe Item)]
childsComp (Item d@Desc {dTip=TCompound, dItems=items} b) = assert ((length items) >= length fspec) (consume items fspec' (B.drop offset b) [])
    where
        (fspec,fspecTotal) = fromMaybe ([],[]) (getFspec b)
        offset = length fspecTotal
        fspec' = fspec ++ (repeat False)

        consume :: [Desc] -> [Bool] -> B.Bits -> [(Name, Maybe Item)] -> [(Name, Maybe Item)]
        consume [] _ _ acc = acc
        consume (i:is) (f:fs) b acc = (name,item):(consume is fs (B.drop size b) acc) where
            name = dName i
            (item,size) = if (f==True) then (Just $ Item i (B.take size b), fromJust $ sizeOf i b)
                            else (Nothing, 0)
childsComp _ = undefined

-- recreate compound item from subitems
unChildsComp :: Desc -> [(Name,Maybe Item)] -> Item
unChildsComp d@Desc {dTip=TCompound, dItems=items} present = assert (length items == length present) $ Item d bs where
    bs = B.pack fspecTotal `mappend` (mconcat . map (encode . fromJust) . filter isJust . map snd $ present)
    fspecTotal
        | fspec == [] = []
        | otherwise = concat leading ++ lastOctet
    leading = map (\l -> l++[True]) (init groups)
    lastOctet = (last groups) ++ [False]
    groups = spl fspec
    spl [] = []
    spl s =
        let (a,b) = splitAt 7 s
        in (fill a):(spl b)
    fill a = take 7 (a++repeat False)
    fspec :: [Bool]
    fspec = strip [isJust . snd $ f | f<-present]
    strip = reverse . dropWhile (==False) . reverse
unChildsComp _ _ = undefined

-- encode item
encode :: Item -> B.Bits
encode (Item _ b) = b

encodeDb :: DataBlock -> B.Bits
encodeDb (DataBlock _ b) = b

-- check that requested number of bits are available
checkSize :: Int -> B.Bits -> Maybe Int
checkSize n b
    | B.length b < n = Nothing
    | otherwise = Just n

-- split datablock to records
toRecords :: [(Category, [(String, Desc)])] -> DataBlock -> Maybe [Item]
toRecords profiles (DataBlock cat bs) = getRecord profiles cat bs [] where
    getRecord profiles' cat' bs' acc = 
        if (B.null bs') then Just . reverse $ acc
        else do
            dsc <- getUapByData cat' profiles' bs'
            size <- sizeOf dsc bs'
            item <- return $ Item dsc (B.take size bs')
            getRecord profiles' cat' (B.drop size bs') (item:acc)

-- read xml (may fail in case of errors in xml)
getCategoryDescription :: String -> (Category, Edition, [(String, Desc)])
getCategoryDescription s = (cat, ed, dscr) where
    name s' = X.blank_name {X.qName=s'}
    elements = X.onlyElems . X.parseXML $ s
    category = head . filter (\e -> (X.qName . X.elName $ e) == "category") $ elements
    cat = getAttr category "cat"
    ed = getAttr category "edition"
    items = map (\i -> (dName (readItem i), readItem i)) . X.elChildren . fromJust . getChild category $ "items"
    dscr = do
        uap <- X.elChildren . fromJust . getChild category $ "uaps"
        let uapName = X.qName . X.elName $ uap
            uapItems = do 
                item <- map X.strContent . X.elChildren $ uap
                return $ case item of
                    "" -> noDesc
                    _ -> fromJust $ lookup item items
            topLevel = Desc {
                dName = ""
                , dTip = TCompound
                , dDsc = "Category " ++ (show cat)
                , dLen = Length0
                , dItems = force uapItems
                , dValue = VRaw
            }
        return (uapName, topLevel)

    getAttr el aName = read . fromJust . X.findAttr (name aName) $ el
    getChild el aName = X.findChild (name aName) $ el

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
        total Desc {dLen=Length2 _ _} = Nothing
        total Desc {dItems=[]} = Just 0
        total dsc@Desc {dItems=(i:is)} = do
            x <- total i
            rest <- total (dsc {dItems=is})
            Just (x + rest)
    
    readItem :: X.Element -> Desc
    readItem e = f dsc' where

        -- check description, recalculate length
        f :: Desc -> Desc

        f dsc@Desc {dTip=TItem, dLen=Length0, dItems=(_:_)} =
            dsc {dLen=recalculateLen dsc}
        f dsc@Desc {dTip=TFixed, dLen=Length1 _, dItems=[]} = dsc
        f dsc@Desc {dTip=TSpare, dLen=Length1 _, dItems=[]} = dsc
        f dsc@Desc {dTip=TExtended, dLen=Length2 _ _} = dsc
        f dsc@Desc {dTip=TRepetitive, dLen=Length0, dItems=(_:_)} = 
            dsc {dLen=recalculateLen dsc}
        f dsc@Desc {dTip=TExplicit, dLen=Length0, dItems=[]} = dsc
        f dsc@Desc {dTip=TCompound, dLen=Length0, dItems=(_:_)} = dsc
        f x = error $ "error in description: " ++ (dName x)

        -- get all elements
        dsc' = Desc {
            dName = fromJust . X.findAttr (name "name") $ e
            , dTip = read . ("T"++) . fromMaybe "Item" . X.findAttr (name "type") $ e
            , dDsc = fromMaybe "" (getChild e "dsc" >>= return . X.strContent)
            , dLen = readLength $ fromMaybe "" (getChild e "len" >>= return . X.strContent)
            , dItems = map readItem . fromMaybe [] $ do
                getChild e "items" >>= return . X.elChildren
            , dValue = getValueTip e
        }

        getValueTip :: X.Element -> Value
        getValueTip el = fromMaybe VRaw $ do
            conv <- getChild el "convert"
            tip <- getChild conv "type" >>= return . X.strContent
            let lsb = getChild conv "lsb" >>= return . fromJust . eval . X.strContent
                unit = getChild conv "unit" >>= return . X.strContent
                min' = getChild conv "min" >>= return . fromJust . eval . X.strContent
                max' = getChild conv "max" >>= return . fromJust . eval . X.strContent
            case tip of
                "string" -> Just VString
                "decimal" -> Just $ VDecimal (fromJust lsb) unit min' max'
                "unsigned decimal" -> Just $ VUnsignedDecimal (fromJust lsb) unit min' max'
                "integer" -> Just $ VInteger unit min' max'
                "unsigned integer" -> Just $ VUnsignedInteger unit min' max'
                _ -> Nothing

getCategoryDescriptionsAll :: [String] -> [(Category, [(Edition, [(String, Desc)])])]
getCategoryDescriptionsAll ss = [(cat, editions cat) | cat <- cats] where
    allStr = sortBy cmp . map getCategoryDescription $ ss
    cats = nub . map (\(c,_,_) -> c) $ allStr
    editions cat = noDups cat . map (\(_,e,d) -> (e,d)) . filter (\(c,_,_) -> (c==cat)) $ allStr
    cmp (c1,e1,_) (c2,e2,_) = (compare c1 c2) `mappend` (compare e1 e2)
    noDups cat edit
        | nub ed == ed = edit
        | otherwise = error $ "duplicated editions in cat: " ++ show cat
        where
            ed = map fst edit

getCategoryDescriptions :: [(Category,Edition)] -> [String] -> [(Category, (Edition, [(String, Desc)]))]
getCategoryDescriptions requested ss = do
    (cat, editions) <- getCategoryDescriptionsAll ss
    let ed = case lookup cat requested of
                Nothing -> last editions
                Just x -> case lookup x editions of
                    Nothing -> error $ "cat: " ++ show cat ++ ", edition: " ++ show x ++ " not found!"
                    Just y -> (x, y)

    return (cat, (fst ed, snd ed))

getUapByName :: Category -> [(Category, [(String, Desc)])] -> String -> Maybe Desc
getUapByName c uaps name = lookup c uaps >>= lookup name

getUapByData :: Category -> [(Category, [(String, Desc)])] -> B.Bits -> Maybe Desc
getUapByData 1 _ _ = undefined  -- TODO
getUapByData cat uaps _ = lookup cat uaps >>= lookup "uap"

getDesc :: Desc -> [String] -> Maybe Desc
getDesc d [] = Just d
getDesc d' (x:xs) = do
    d1 <- findEl x d'
    getDesc d1 xs where
        findEl name dsc = lookup name $ [(dName d,d) | d <- dItems dsc]

getDesc' :: Desc -> [String] -> Desc
getDesc' d s = fromJust $ getDesc d s

main :: IO ()
main = do
    return  ()

