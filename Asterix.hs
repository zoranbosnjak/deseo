
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
    , getUapByName
    , getUapByData
    -- , decode
    , encode 
    , DataBlock(..)
    , toDataBlocks
    --, toRecords
    , getDesc
    , getDesc'
    -- , subitem
    --, item
    --, Content(..)
    --, toContent
    , create
    , fromValue
    , fromValues
    , fromList
    , setItem
    , sizeOf
    , child
    , childR
    , childsRep
    , childsComp
    , unChildsComp
    , getFspec
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

import qualified Bits as B

import Debug.Trace

dump = flip trace

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

data Desc = Desc {  dName       :: String
                    , dTip      :: Tip
                    , dDsc      :: String
                    , dLen      :: Length
                    , dItems    :: [Desc]

                    -- TODO: convert functions
                    {-
                        use:
                            class Value
                                toValue
                                fromValue

                        see:
                            http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html
                            http://book.realworldhaskell.org/read/using-typeclasses.html
                    -}
                 } deriving (Eq)

instance NFData Desc

instance Show Desc where
    show d = (show . dTip $ d) ++ " (" ++ (dName d) ++ "), " ++ (show . dLen $ d)

noDesc = Desc {
    dName = ""
    , dTip = TItem
    , dDsc = ""
    , dLen = Length0
    , dItems = []
}

data Item = Item Desc B.Bits deriving (Show,Eq)

-- create item helper functions
-- f :: Content -> Desc -> Item
-- if a content is applied to the function, the return
-- value is a general transform function from Desc -> Item

fromValue :: Integral a => a -> Desc -> Item
fromValue val d@Desc {dLen=Length1 len} = case (dTip d) of
    TItem -> Item d (B.bits len val)
    TFixed -> Item d (B.bits len val)

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
    if (B.take size b) /= (B.zeros size) then Nothing
    else Just size

-- size of Extended
sizeOf d@Desc {dTip=TExtended, dLen=Length2 n1 n2} b = do
    size <- checkSize n1 b
    if (B.index b (size-1)) then dig size
    else Just size
    where
        dig offset = do 
            size <- checkSize offset b
            if (B.index b (size-1)) then dig (size+n2)
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
    if (last val == False) then Just ((init val), val)
    else do
        (rem, remTotal) <- getFspec (B.drop n b)
        let rv = (init val) ++ rem
            rvTotal = val ++ remTotal
        Just (rv, rvTotal)

-- get subitem
child :: Name -> Item -> Maybe Item
child = undefined

-- get deep subitem, like ["010", "SAC"]
childR :: [Name] -> Item -> Maybe Item
childR [] item = Just item
childR (i:is) item = do
    c <- child i item
    childR is c

-- get repetitive items
childsRep :: Item -> Maybe [Item]
childsRep (Item d@Desc {dTip=TRepetitive} b) = undefined

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

-- encode item
encode :: Item -> B.Bits
encode (Item _ b) = b

{-
item :: Desc -> Content -> Item
item d@Desc {dTip=TItem, dItems=subitems} c@(CList (IList [items])) = Item d c
item d@Desc {dTip=TFixed} c@(CBits b) = Fixed d c
item _ _ = undefined

-- newItem d@Desc {dTip=TItem, dItems=subitems} [items] = undefined
-- newItem d@Desc {dTip=TCompound, dItems=items} = (Compound . replicate (length items) $ Nothing, d)


-- like decode, but given bits must be exact in size and decoding must be possible
decodeExact :: Desc -> B.Bits -> Item
decodeExact d b = assert (size==B.length b) item where
    (item,size) = fromJust $ decode d b

-- decode items
decode :: Desc -> B.Bits -> Maybe (Item, Size)

-- decode Item (decode first subitem, decode the rest, then combine)
decode d@Desc {dTip=TItem, dItems=[]} _ = Just (Item d (CList . IList $ []), 0)
decode d@Desc {dTip=TItem, dItems=(i:is)} b = do
    (x,s1) <- decode i b
    (Item d2 y,s2) <- decode (d {dItems=is}) (B.drop s1 b)
    let size = case (dLen d) of
                -- if the size is known, take it
                (Length1 n) -> n
                otherwise -> s1+s2
        CList y'' = y
        y' = fromIList y''
    Just (Item d (CList . IList $ (x:y')), size)

-- decode Fixed
decode d@Desc {dTip=TFixed, dLen=Length1 n} b = do
    size <- checkSize n b
    Just (Fixed d $ CBits $ B.take size b, size)

-- decode Spare
decode d@Desc {dTip=TSpare, dLen=Length1 n} b = do
    size <- checkSize n b
    if (B.take size b) /= (B.zeros size) then Nothing
    else Just (Spare d (CInt size), size)

-- decode Extended
decode d@Desc {dTip=TExtended, dLen=Length2 n1 n2} b = do
    size <- checkSize n1 b
    if (B.index b (size-1)) then dig size
    else Just (fetch size, size)
        where
            fetch size = Extended d . CData . Raw . B.take size $ b
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
    Just (Repetitive d (CList . IList . reverse $ items), calcSize size rep) where
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
decode d@Desc {dTip=TExplicit} b = do
    s8 <- checkSize 8 b
    let val = B.toUnsigned . B.take s8 $ b
    case val of
        0 -> Nothing
        otherwise -> do
            size <- checkSize (8*val) b
            Just (Explicit d . CBits . B.take size $ b, size)

-- decode Compound
decode d@Desc {dTip=TCompound, dItems=items} b' = do
    b <- B.checkAligned b'
    (fspec, fspecTotal) <- getFspec b
    -- TODO: check length of items (must be >= length of fspec)
    let subitems :: [(String,Desc)]
        subitems = [(dName dsc,dsc) | (f,dsc) <- zip fspec items, (f==True)]
        offset = length fspecTotal

    (items, size) <- dig subitems (B.drop offset b) [] offset
    Just (Compound d (CMap . IMap $ items), size) where

        dig :: [(String,Desc)] -> B.Bits -> [(String,Item)] -> Size -> Maybe ([(String,Item)], Size)
        dig [] _ acc size = Just (acc, size)
        dig (x:xs) b acc size = do
            (item, itemSize) <- decode (snd x) b
            let item' = (fst x, item)
            dig xs (B.drop itemSize b) (item':acc) (itemSize+size)

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
-}

-- check that requested number of bits are available
checkSize :: Int -> B.Bits -> Maybe Int
checkSize n b
    | B.length b < n = Nothing
    | otherwise = Just n

-- read xml (may fail in case of errors in xml)
getCategoryDescription :: String -> (Category, Edition, [(String, Desc)])
getCategoryDescription s = (cat, ed, dsc) where
    name s = X.blank_name {X.qName=s}
    elements = X.onlyElems . X.parseXML $ s
    category = head . filter (\e -> (X.qName . X.elName $ e) == "category") $ elements
    cat = getAttr category "cat"
    ed = getAttr category "edition"
    items = map (\i -> (dName (readItem i), readItem i)) . X.elChildren . fromJust . getChild category $ "items"
    dsc = do
        uap <- X.elChildren . fromJust . getChild category $ "uaps"
        let uapName = X.qName . X.elName $ uap
            uapItems = do 
                item <- map X.strContent . X.elChildren $ uap
                return $ case item of
                    "" -> noDesc
                    otherwise -> fromJust $ lookup item items
            topLevel = Desc {
                dName = ""
                , dTip = TCompound
                , dDsc = "Category " ++ (show cat)
                , dLen = Length0
                , dItems = force uapItems
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
        total Desc {dLen=Length2 a b} = Nothing
        total Desc {dItems=[]} = Just 0
        total dsc@Desc {dItems=(i:is)} = do
            x <- total i
            rest <- total (dsc {dItems=is})
            Just (x + rest)
    
    readItem :: X.Element -> Desc
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
            dName = fromJust . X.findAttr (name "name") $ e
            , dTip = read . ("T"++) . fromMaybe "Item" . X.findAttr (name "type") $ e
            , dDsc = fromMaybe "" (getChild e "dsc" >>= return . X.strContent)
            , dLen = readLength $ fromMaybe "" (getChild e "len" >>= return . X.strContent)
            , dItems = map readItem . fromMaybe [] $ do
                getChild e "items" >>= return . X.elChildren
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

getUapByName :: Category -> [(Category, [(String, Desc)])] -> String -> Maybe Desc
getUapByName cat uaps name = lookup cat uaps >>= lookup name

getUapByData :: Category -> [(Category, [(String, Desc)])] -> B.Bits -> Maybe Desc
getUapByData 1 uaps b = undefined
getUapByData cat uaps _ = lookup cat uaps >>= lookup "uap"

{-
-- split datablock to records
toRecords :: [(Category, [(String, Desc)])] -> DataBlock -> Maybe [Item]
toRecords profiles (DataBlock cat bs) = getRecord profiles cat bs [] where
    getRecord profiles cat bs acc = 
        if (B.null bs) then Just . reverse $ acc
        else do
            dsc <- getUapByData cat profiles bs
            (item,size) <- decode dsc bs
            getRecord profiles cat (B.drop size bs) (item:acc)

subitem :: [String] -> (Item, Desc) -> Maybe (Item, Desc)
subitem [] (item, dsc) = Just (item,dsc)
subitem (x:xs) (item, dsc) = do
    ix <- elemIndex x (map dName (dItems dsc))
    item2 <- getSubItem item ix
    let dsc2 = (dItems dsc) !! ix
    subitem xs (item2, dsc2) where
        getSubItem (Item items) ix = Just (items !! ix)
        getSubItem (Compound maybeItems) ix = maybeItems !! ix
-}  

getDesc :: Desc -> [String] -> Maybe Desc
getDesc d [] = Just d
getDesc d (x:xs) = do
    d1 <- find x d
    getDesc d1 xs where
        find name dsc = lookup name $ [(dName d,d) | d <- dItems dsc]

getDesc' d s = fromJust $ getDesc d s

main :: IO ()
main = do
    return  ()

