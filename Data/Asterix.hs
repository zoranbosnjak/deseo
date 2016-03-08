{-# LANGUAGE DeriveGeneric #-}

----------------
-- |
-- Module       :  Data.Asterix
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides encoder/decoder for Asterix data.
--
-- > import Data.Asterix as A
--
-- Examples:
--
--      * parse XML
--  
-- >        s <- readFile "path/to/catXY.xml"
-- >        let c = categoryDescription s
--
--      * parse many XML files
--        
-- >        import Control.Exception (evaluate)
-- >        import Control.DeepSeq (force)
-- >
-- >        -- read files, force evaluation
-- >        let files = [fileName1, fileName2,...]
-- >        descriptions <- forM files $ \i -> do
-- >        xml <- readFile i
-- >        case categoryDescription xml of
-- >            Left e -> error $ i ++ ", " ++ e
-- >            Right val -> return val >>= evaluate . force
-- >
-- >        -- get specific revision for cat 8, and latest for other categories
-- >        uaps <- case categorySelect descriptions [(8, Edition 1 2)] of
-- >            Left e -> error e
-- >            Right val -> return val

--
--      * decode bits to records
--
-- >        import qualified Data.BitString as B
-- >
-- >        let datablock = B.fromInteger 48 0x000006800203
-- >
-- >        profiles <- ...
-- >        let parse db = return db
-- >                >>= toDataBlocks
-- >                >>= mapM (toRecords profiles)
-- >                >>= return . join
-- >
-- >        print $ parse datablock
--
--      * decode items
--
--          TODO
--
--      * building items
--
-- >        catXY <- ...
-- >        rec <- create catXY $ do
-- >            "010" <! fromBits (B.fromInteger 16 0x0102)
-- >            "020" <! fromRawInt 0x0203
-- >            "030" <! fromValues fromRawInt [("SAC", 0x01), ("SIC", 0x02)]
-- >           
-- >        rec <- fromValues fromRawInt [("010", 0x0102)] catXY
--
--
--      * encode
--
--          TODO
--

module Data.Asterix
(   
    -- * Data types
    Tip(..)
    , Item(..)
    , Desc(..)
    , Category(..), Edition(..), Uap

    -- * Aliases
    , UapName, ItemName, ItemDescription
    , Major, Minor
    , Cat
    , Profiles

    -- * Datablock
    , DataBlock(..)
    , toDataBlocks
    , datablock 

    -- * XML parsers
    , categoryDescription
    , categorySelectAll
    , categorySelect

    -- * UAP
    , uapByName
    , uapByData

    -- * Decode
    , toRecords

    -- * Subitem access
    , child
    , childR
    , childs
    , unChilds

    -- * Building items
    , create, (<!), putItem, delItem, failItem

    -- * Converters: value -> Maybe Item
    , fromBits
    , fromRaw
    , fromRawInt
    , fromRawInteger
    , fromNatural
    --, fromString
    , fromValues

    -- * Converters: Item -> Maybe value
    , toBits
    , toRaw
    , toNatural
    --, toString

    -- * Util functions
    , sizeOf
    , grab

    -- * Expression evaluation
    , eval
    , EValue(..)

) where

import Data.Function (on)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.List (dropWhileEnd, nub, sortBy)
import qualified Data.Map as Map
import Data.Word

import Control.DeepSeq.Generics
import Control.Monad
import Control.Monad.State
import GHC.Generics

import qualified Text.XML.Light as X
import Text.XML.Light.Lexer (XmlSource)

import qualified Data.BitString as B
import Data.Asterix.Expression

-- for debug purposes
--import Debug.Trace
--dump = flip trace

-- | Asterix item types
data Tip = TItem
           | TFixed
           | TSpare
           | TExtended
           | TExtendedVariant
           | TRepetitive
           | TExplicit
           | TCompound
           | TRfs
           deriving (Show, Read, Eq, Generic)
instance NFData Tip

-- | description + data
data Item = Item {
        iDsc :: Desc 
        , iBits :: B.Bits 
        } deriving (Show, Eq)

-- | Asterix item description
data Desc = Desc {  dName       :: ItemName
                    , dTip      :: Tip
                    , dDsc      :: ItemDescription
                    , dLen      :: Length
                    , dItems    :: [Desc]
                    , dValue    :: Value
                 } deriving (Eq, Generic)
instance NFData Desc
instance Show Desc where
    show d = (show . dTip $ d) ++ " (" ++ (dName d) ++ "), " ++ (show . dLen $ d) ++ ", " ++ (show $ dValue d)

-- | Empty description
noDesc :: Desc
noDesc = Desc {
    dName = ""
    , dTip = TItem
    , dDsc = ""
    , dLen = Length0
    , dItems = []
    , dValue = VRaw
}

type Cat = Word8
type Uap = (UapName,Desc)
type UapName = String
type ItemName = String
type ItemDescription = String
type Major = Int
type Minor = Int
type Size = Int

-- | Asterix standard (particular edition)
data Category = Category {
        cCat :: Cat 
        ,cEdition :: Edition 
        ,cUaps :: [Uap]
    } deriving (Generic)
instance NFData Category
instance Show Category where
    show c = "(category " ++ (show $ cCat c) ++ ", edition " ++ (show $ cEdition c) ++ ")"

type Profiles = Map.Map Cat Category

-- | Asterix edition
data Edition = Edition Major Minor deriving (Eq, Generic)
instance NFData Edition
instance Ord Edition where
    compare (Edition a1 b1) (Edition a2 b2) = (compare a1 a2) `mappend` (compare b1 b2)
instance Show Edition where
    show (Edition a b) = show a ++ "." ++ show b
instance Read Edition where
    readsPrec _ value = [(Edition (read a) (read b), "")] where
        a = takeWhile (/='.') value
        b = tail . dropWhile (/='.') $ value

-- | Length of asterix item
data Length = Length0 | Length1 Int | Length2 Int Int deriving (Show, Read, Eq, Generic)
instance NFData Length

type Lsb = EValue
type Unit = String
type Min = EValue
type Max = EValue

data Value = 
    VRaw
    | VString
    | VDecimal Lsb (Maybe Unit) (Maybe Min) (Maybe Max)
    | VUnsignedDecimal Lsb (Maybe Unit) (Maybe Min) (Maybe Max)
    | VInteger (Maybe Unit) (Maybe Min) (Maybe Max)
    | VUnsignedInteger (Maybe Unit) (Maybe Min) (Maybe Max)
    deriving (Eq, Show, Generic)
instance NFData Value

data DataBlock = DataBlock {
    dbCat :: Cat
    , dbData :: B.Bits
} deriving (Eq, Show)

--  | Create datablock
datablock :: Cat -> [Item] -> DataBlock
datablock cat items = DataBlock {dbCat=cat, dbData=bs} where
    bs = c `mappend` ln `mappend` records
    c = B.fromInteger 8 $ toInteger cat
    ln = B.fromInteger 16 $ fromIntegral $ (B.length records `div` 8) + 3
    records = mconcat $ map iBits items

-- | Request particular edition of a category or latest
categorySelect :: [Category] -> [(Cat,Edition)] -> Either String Profiles
categorySelect dsc requested = do
    dsc' <- categorySelectAll dsc
    p <- forM (Map.toList dsc') $ \(cat,editions) -> do
        case lookup cat requested of
            Nothing -> Right $ (cat, last editions)
            Just ed -> case lookup ed [(cEdition c, c) | c <- editions] of
                Nothing -> Left $ "Cat " ++ (show cat) ++ ", edition " ++ (show ed) ++ " not found!"
                Just val -> Right (cat, val)
    Right $ Map.fromList p

-- | Group descriptions by category and sort by edition
categorySelectAll :: [Category] -> Either String (Map.Map Cat [Category])
categorySelectAll dsc = do
    let cats = nub . map cCat $ dsc
    p <- forM cats $ \cat -> do
        let editions = sortBy (compare `on` cEdition) . filter ((==cat) . cCat) $ dsc
            eds = map cEdition editions
        case nub eds == eds of
            True -> Right $ (cat, editions)
            False -> Left $ "duplicated editions in cat: " ++ show cat
    Right $ Map.fromList p

-- | Read xml content.
categoryDescription :: XmlSource s => s -> Either String Category
categoryDescription src = do
    let elements = X.onlyElems . X.parseXML $ src
     
    category <- case filter (\e -> (X.qName . X.elName $ e) == "category") $ elements of
                    [] -> Left "<category> not found in xml"
                    (x:_) -> Right x

    cat <- getAttr category "cat"
    ed <- getAttr category "edition"

    items <- getChild category "items"
                >>= return . X.elChildren
                >>= mapM readItem
                >>= return . map (\i -> (dName i, i))

    dscr <- do
        uaps <- getChild category "uaps" >>= return . X.elChildren
        forM uaps $ \uap -> do
            uapItems <- forM (map X.strContent . X.elChildren $ uap) $ \item -> do
                case item of
                    "" -> Right noDesc
                    _ -> case (lookup item items) of
                        Nothing -> Left $ "item " ++ (show item) ++ " not found in <items>"
                        Just x -> Right x
            let uapName = X.qName . X.elName $ uap
                topLevel = Desc {
                    dName = ""
                    , dTip = TCompound
                    , dDsc = "Category " ++ (show cat)
                    , dLen = Length0
                    , dItems = uapItems
                    , dValue = VRaw
                }
            return (uapName, topLevel)

    Right $ Category {cCat=cat, cEdition=ed, cUaps=dscr} 
    
    where
        nameOf s = X.blank_name {X.qName=s}

        getAttr el aName = case X.findAttr (nameOf aName) el of
                            Nothing -> Left $ "Attribute '"++aName++ "' not found in element " ++ (show el)
                            Just x -> Right $ read x

        getChild el aName = case X.findChild (nameOf aName) el of
                            Nothing -> Left $ "Child '"++aName++ "' not found in element " ++ (show el)
                            Just x -> Right x
    
        readLength :: String -> Either String Length
        readLength s
            | s == "" = Right Length0
            | isJust (maybeRead s :: Maybe Size) = Right $ Length1 . read $ s
            | isJust (maybeRead s :: Maybe (Size,Size)) = Right $ Length2 a b
            | otherwise = Left $ "Unable to read length: " ++ s
            where
                (a,b) = read s
                maybeRead s' = case reads s' of
                    [(x, "")] -> Just x
                    _         -> Nothing

        recalculateLen :: Desc -> Length
        recalculateLen dsc = fromMaybe Length0 (total dsc >>= Just . Length1) where
            total :: Desc -> Maybe Size
            total Desc {dLen=Length1 a} = Just a
            total Desc {dLen=Length2 _ _} = Nothing
            total Desc {dItems=[]} = Just 0
            total dsc'@Desc {dItems=(i:is)} = do
                x <- total i
                rest <- total (dsc' {dItems=is})
                Just (x + rest)
        
        readItem :: X.Element -> Either String Desc
        readItem el = dsc' el >>= f where 

            -- check description, recalculate length
            f :: Desc -> Either String Desc
            f dsc@Desc {dTip=TItem, dLen=Length0} = Right $ dsc {dLen=recalculateLen dsc}
            f dsc@Desc {dTip=TFixed, dLen=Length1 _, dItems=[]} = Right dsc
            f dsc@Desc {dTip=TSpare, dLen=Length1 _, dItems=[]} = Right dsc
            f dsc@Desc {dTip=TExtended, dLen=Length2 _ _} = Right dsc
            f dsc@Desc {dTip=TExtendedVariant, dLen=Length2 _ _} = Right dsc
            f dsc@Desc {dTip=TRepetitive, dLen=Length0, dItems=(_:_)} = Right $ dsc {dLen=recalculateLen dsc}
            f dsc@Desc {dTip=TExplicit, dLen=Length0, dItems=[]} = Right dsc
            f dsc@Desc {dTip=TCompound, dLen=Length0, dItems=(_:_)} = Right dsc
            f dsc@Desc {dTip=TRfs, dLen=Length0, dItems=[]} = Right dsc
            f x = Left $ "error in description: " ++ (dName x)

            -- get all elements
            dsc' e = do
                name <- case X.findAttr (nameOf "name") e of
                    Nothing -> Right ""
                    Just n -> Right n
                tip <- return . read . ("T"++) . fromMaybe "Item" . X.findAttr (nameOf "type") $ e
                dsc <- return $ either (\_->"") (X.strContent) (getChild e "dsc")
                len <- return (either (\_->"") (X.strContent) (getChild e "len")) >>= readLength
                items <- sequence $ map readItem $ either (\_->[]) X.elChildren (getChild e "items")
                val <- getValueTip e

                Right $ Desc {dName=name, dTip=tip, dDsc=dsc, dLen=len, dItems=items, dValue=val}

            getValueTip :: X.Element -> Either String Value
            getValueTip el' = case getChild el' "convert" of
                Left _ -> Right VRaw
                Right conv -> do
                    tip <- getChild conv "type" >>= return . X.strContent

                    let tryGetChild element name = case getChild element name of
                                Left _ -> Right Nothing
                                Right ch -> Right . Just $ X.strContent ch
                        tryEval exp' = case exp' of
                                Nothing -> Right Nothing
                                Just exp'' -> case eval exp'' of
                                    Nothing -> Left $ "unable to eval " ++ exp''
                                    Just val -> Right $ Just val

                    lsb <- tryGetChild conv "lsb" >>= tryEval
                    unit <- tryGetChild conv "unit" 
                    min' <- tryGetChild conv "min" >>= tryEval
                    max' <- tryGetChild conv "max" >>= tryEval

                    case tip of
                        "string" -> Right VString
                        "decimal" -> case lsb of
                                        Nothing -> Left $ "'lsb' missing " ++ (show el')
                                        Just lsb' -> Right $ VDecimal lsb' unit min' max'
                        "unsigned decimal" -> case lsb of
                                        Nothing -> Left $ "'lsb' missing " ++ (show el')
                                        Just lsb' -> Right $ VUnsignedDecimal lsb' unit min' max'
                        "integer" -> Right $ VInteger unit min' max'
                        "unsigned integer" -> Right $ VUnsignedInteger unit min' max'
                        _ -> Right VRaw

-- | get UAP by name
uapByName :: Category -> UapName -> Maybe Desc
uapByName c name = lookup name (cUaps c)

-- | get UAP by data
uapByData :: Category -> B.Bits -> Maybe Desc
uapByData c _
    | cCat c == 1   = undefined -- TODO, cat1 is special
    | otherwise     = uapByName c "uap"

-- | Split bits to datablocks
toDataBlocks :: B.Bits -> Maybe [DataBlock]
toDataBlocks bs
    | B.null bs = Just []
    | otherwise = do
        x <- B.checkAligned bs
        cat <- return x >>= B.takeMaybe 8 >>= return . B.toUIntegral
        len <- return x >>= B.dropMaybe 8 >>= B.takeMaybe 16 >>= return . B.toUIntegral
        y <- return x >>= B.takeMaybe (len*8) >>= B.dropMaybe 24

        let db = DataBlock cat y
        rest <- return x >>= B.dropMaybe (len*8) >>= toDataBlocks
        Just (db:rest)

-- | Split datablock to records.
toRecords :: Profiles -> DataBlock -> Maybe [Item]
toRecords profiles db = do
    let cat = dbCat db
        d = dbData db
    category <- Map.lookup cat profiles
    getRecords category d 
    where

        getRecords :: Category -> B.Bits -> Maybe [Item]
        getRecords category bs
            | B.null bs = Just []
            | otherwise = do
                dsc <- uapByData category bs
                size <- sizeOf dsc bs
                rec <- B.takeMaybe size bs
                rest <- getRecords category (B.drop size bs)
                Just $ (Item {iDsc=dsc, iBits=rec}):rest

-- | Get fspec bits, (without fs, with fx)
getFspec :: B.Bits -> Maybe ([Bool],[Bool])
getFspec b = do
    n <- checkSize 8 b
    let val = B.unpack . B.take n $ b
    if (last val == False)
        then Just ((init val), val)
        else do
            (remin, remTotal) <- getFspec (B.drop n b)
            let rv = (init val) ++ remin
                rvTotal = val ++ remTotal
            Just (rv, rvTotal)

-- | Check that requested number of bits are available.
checkSize :: Int -> B.Bits -> Maybe Int
checkSize n b
    | B.length b < n = Nothing
    | otherwise = Just n

-- | Grab bits: bits -> (bits for item, remaining bits)
grab :: Desc -> B.Bits -> Maybe (B.Bits, B.Bits)
grab dsc b = do
    size <- sizeOf dsc b
    Just (B.take size b, B.drop size b)

-- | Calculate items size.
sizeOf :: Desc -> B.Bits -> Maybe Size

-- size of Item
sizeOf Desc {dTip=TItem, dLen=Length1 n} b = checkSize n b
sizeOf Desc {dTip=TItem, dItems=[]} _ = Just 0
sizeOf d@Desc {dTip=TItem, dItems=(i:is)} b = do
    size <- sizeOf i b
    rest <- sizeOf (d {dItems=is}) (B.drop size b)
    Just (size+rest)

-- size of Fixed
sizeOf Desc {dTip=TFixed, dLen=Length1 n} b = checkSize n b

-- size of Spare
sizeOf Desc {dTip=TSpare, dLen=Length1 n} b = do
    size <- checkSize n b
    if (B.take size b) /= (B.zeros size) 
        then Nothing
        else Just size

-- size of Extended
sizeOf Desc {dTip=TExtended, dLen=Length2 n1 n2} b = do
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

-- size of ExtendedVariant (primary + maximum one extension)
sizeOf Desc {dTip=TExtendedVariant, dLen=Length2 n1 n2} b = do
    size <- checkSize n1 b
    case (B.index b (size-1)) of
        False -> Just size
        True -> checkSize (n1+n2) b

-- size of Repetitive
sizeOf d@Desc {dTip=TRepetitive} b = do
    s8 <- checkSize 8 b
    let rep = B.toUIntegral . B.take s8 $ b
        b' = B.drop s8 b
    getSubitems rep b' 8
    where
        getSubitems :: Int -> B.Bits -> Size -> Maybe Size
        getSubitems 0 _ size = Just size
        getSubitems n b'' size = do
            itemSize <- sizeOf (d {dTip=TItem}) b''
            getSubitems (n-1) (B.drop itemSize b'') (itemSize+size)

-- size of Explicit
sizeOf Desc {dTip=TExplicit} b = do
    s8 <- checkSize 8 b
    let val = B.toUIntegral . B.take s8 $ b
    case val of
        0 -> Nothing
        _ -> checkSize (8*val) b

-- size of Compound
sizeOf Desc {dTip=TCompound, dItems=items} b' = do
    b <- B.checkAligned b'
    let (fspec, fspecTotal) = fromMaybe ([],[]) (getFspec b)
        subitems = [(dName dsc,dsc) | (f,dsc) <- zip fspec items, (f==True)]
        offset = length fspecTotal
        fspecMin = dropWhileEnd (==False) fspec

    guard (length items >= length fspecMin)
    dig subitems (B.drop offset b) offset
    where
        dig [] _ size = Just size
        dig (x:xs) b size = do
            itemSize <- sizeOf (snd x) b
            dig xs (B.drop itemSize b) (itemSize+size)

-- size of unknown
sizeOf _ _ = Nothing

-- | Get subitem.
--
-- >    return item >>= child "010" >>= child "SAC"
--
child :: ItemName -> Item -> Maybe Item
child name item = childs item >>= return . lookup name >>= join

-- | Get deep subitem.
--
-- >    childR ["010", "SAC"] item
--
childR :: [ItemName] -> Item -> Maybe Item
childR [] item = Just item
childR (i:is) item = child i item >>= childR is

-- | Get all subitems.
childs :: Item -> Maybe [(ItemName,Maybe Item)]
childs item

    -- Item
    | tip == TItem = do 
        let consume [] _ = Just []
            consume (i:is) b' = do
                (x,y) <- grab i b'
                rest <- consume is y
                Just $ (dName i, Just $ Item i x):rest
        consume items b

    -- Extended
    | tip == TExtended = do
        let dsc = iDsc item
            Length2 n1 n2 = dLen dsc
            chunks = [n1] ++ repeat n2

            -- if no more items, there must be also end of a chunk
            consume [] (c:_) n _
                | (n+1) == c = Just []
                | otherwise = Nothing

            -- consume next item
            consume items'@(i:is) chunks'@(c:cs) n b'

                -- overflow
                | (n+1) > c = Nothing

                -- end of chunk
                | (n+1) == c = do
                    fx <- B.takeMaybe 1 b'
                    y <- B.dropMaybe 1 b'
                    case B.anySet fx of
                        False -> Just [(dName x,Nothing) | x<-items']
                        True -> consume items' cs 0 y

                -- consume next item
                | otherwise = do
                    (x,y) <- grab i b'
                    rest <- consume is chunks' (n + B.length x) y
                    Just $ (dName i, Just $ Item i x):rest

            consume _ _ _ _ = Nothing

        consume items chunks 0 b

    -- ExtendedVariant
    | tip == TExtendedVariant = do
        let dsc = iDsc item
            Length2 n1 n2 = dLen dsc

            consumePrim _ [] _ = Just []
            consumePrim n (i:is) b'
                | n<1 = Nothing
                | n==1 = do
                    fx <- B.takeMaybe 1 b'
                    b'' <- B.dropMaybe 1 b'
                    case B.anySet fx of
                        False -> Just []
                        True -> consumeSec n2 (i:is) b''
                | otherwise = do
                    (x,y) <- grab i b'
                    rest <- consumePrim (n - B.length x) is y
                    Just $ (dName i, Just $ Item i x):rest

            consumeSec _ [] _ = Just []
            consumeSec n (i:is) b'
                | n<0 = Nothing
                | n==0 = Just []
                | otherwise = do
                    (x,y) <- grab i b'
                    rest <- consumeSec (n - B.length x) is y
                    Just $ (dName i, Just $ Item i x):rest

        consumePrim n1 items b

    -- Repetitive
    | tip == TRepetitive = undefined    -- TODO

    -- Compound
    | tip == TCompound = do
        let (fspec, fspecTotal) = fromMaybe ([],[]) (getFspec b)

        let fspec' = fspec ++ (repeat False)

            consume [] _ = Just []
            consume ((i,f):xs) bs = do
                (item',b') <- fetch f
                rest <- consume xs b'
                Just $ (name,item'):rest
                where
                    name = dName i
                    fetch False = Just (Nothing, bs)
                    fetch True = do
                        (x,y) <- grab i bs
                        Just $ (Just $ Item i x, y)

            checkFspec
                | (length minFspec) <= (length items) = Just minFspec
                | otherwise = Nothing
                where minFspec = reverse . dropWhile (==False) . reverse $ fspec

        _ <- checkFspec
        b' <- B.dropMaybe (length fspecTotal) b
        consume (zip items fspec') b'

    -- unknown
    | otherwise = Nothing
    where
        tip = dTip . iDsc $ item
        items = dItems . iDsc $ item
        b = iBits item

-- recreate item from subitems
unChilds :: Desc -> [(ItemName,Maybe Item)] -> Maybe Item
unChilds dsc present = case (dTip dsc) of
    TCompound -> do
        let items = dItems dsc
            bs = B.pack fspecTotal `mappend` (mconcat . map iBits . catMaybes . map snd $ present)
            fspecTotal
                | fspec == [] = []
                | otherwise = concat leading ++ lastOctet
            fspec = dropWhileEnd (==False) $ [isJust . snd $ f | f<-present]
            leading = map (++[True]) (init groups)
            lastOctet = (last groups) ++ [False]
            groups = spl fspec
            spl [] = []
            spl s =
                let (a,b) = splitAt 7 s
                in (fill a):(spl b)
            fill a = take 7 (a++repeat False)
        guard $ length items == length present
        return $ Item dsc bs
    _ -> Nothing

-- | Create compound item from profile and transform function.
create :: Desc -> State (Maybe Item) () -> Maybe Item
create dsc transform
    | tip==TCompound = execState transform $ emptyRecord dsc
    | otherwise = Nothing
    where
        tip = dTip dsc
        emptyRecord d = Just $ Item d mempty

-- | Alias for 'putItem'.
--
-- >    rec <- create cat $ do
-- >        "010" <! fromRaw 0x0102
-- >        (<!) "030" $ fromRaw 0x0102
--
(<!) :: String -> (Desc -> Maybe Item) -> State (Maybe Item) ()
(<!) = putItem

-- | Put subitem to compound item (stateful computation).
--
-- >    rec <- create cat $ do
-- >        "020" `putItem` fromRaw 0x0102
-- >        putItem "040" $ fromRaw 0x0102
--
putItem :: String -> (Desc -> Maybe Item) -> State (Maybe Item) ()
putItem name toItem = state $ \i -> ((),newItem i) where
    newItem Nothing = Nothing
    newItem (Just parent) = case (dTip . iDsc $ parent) of
        TCompound -> do
            dsc <- lookup name [(dName d, d) | d <- dItems . iDsc $ parent]
            let maybeItem = toItem dsc
                replace l = do
                    (a,b) <- l
                    case (a==name) of
                        True -> [(a,maybeItem)]
                        False -> [(a,b)]
            guard $ isJust maybeItem
            childs parent
                >>= return . replace
                >>= unChilds (iDsc parent)
        _ -> Nothing

-- | Force computation to fail
--
-- >    rec <- create cat $ do
-- >        "020" `putItem` fromRaw 0x0102
-- >        failItem
-- >        "030" `putItem` fromRaw 0x0304
--
failItem :: State (Maybe Item) ()
failItem = state $ \_ -> ((), Nothing)

-- | Delete subitem from compound item.
delItem :: String -> State (Maybe Item) ()
delItem = undefined -- TODO

-- convert functions: 

_chkLimit :: Maybe t -> (a -> t -> Bool) -> a -> Maybe a
_chkLimit Nothing _ val = Just val
_chkLimit (Just limit) cmp val
    | val `cmp` limit = Nothing
    | otherwise = Just val

fromBits :: B.Bits -> Desc -> Maybe Item
fromBits val dsc@Desc {dLen=Length1 len} = do
    guard (B.length val==len)
    return $ Item dsc val
fromBits val dsc = case (dTip dsc) of
    TExplicit -> do
        let (a,b) = divMod (B.length val) 8
            ln = fromIntegral $ a+1
            size = B.fromInteger 8 ln
        guard (b==0)
        guard (ln<=255)
        return $ Item dsc (size `mappend` val)
    _ -> undefined -- TODO: take value, decode, encode, check

-- | Convert from raw value (item size must be known).
fromRaw :: Integral a => a -> Desc -> Maybe Item
fromRaw val dsc@Desc {dLen=Length1 len} = Just $ Item dsc $ B.fromInteger len $ fromIntegral val
fromRaw _ _ = Nothing

fromRawInt :: Int -> Desc -> Maybe Item
fromRawInt = fromRaw 

fromRawInteger :: Integer -> Desc -> Maybe Item
fromRawInteger = fromRaw 

-- | Convert from natural value.
fromNatural :: EValue -> Desc -> Maybe Item
fromNatural val dsc@Desc {dLen=Length1 len} = do
    let ival (EInteger x) = x
        ival (EDouble x) = truncate x
    (conv,mmin,mmax) <- case dValue dsc of
        VDecimal lsb _ mmin mmax -> Just ((/lsb), mmin, mmax)
        VUnsignedDecimal lsb _ mmin mmax ->
            if val >= 0
                then Just ((/lsb), mmin, mmax)
                else Nothing
        VInteger _ mmin mmax -> Just (id, mmin, mmax)
        VUnsignedInteger _ mmin mmax ->
            if val >= 0
                then Just (id, mmin, mmax)
                else Nothing
        _ -> Nothing
    val' <- return val >>= _chkLimit mmin (<) >>= _chkLimit mmax (>) >>= return . ival . conv
    return $ Item dsc (B.fromInteger len val')
fromNatural _ _ = Nothing

-- | Convert from given values, convert each, then apply
fromValues :: (a -> Desc -> Maybe Item) -> [(ItemName, a)] -> Desc -> Maybe Item
fromValues f list parentDsc = case (dTip parentDsc) of

    TItem -> do
        guard $ names1 == names2
        l <- sequence [f val d | (val,d) <- zip values items]
        return $ Item parentDsc (mconcat . map iBits $ l)

    TCompound -> do
        let transform = foldr (>>) noChange transforms
            noChange = state $ \i -> ((), i)
            transforms = [putItem name (f val) | (name,val) <- list]
        create parentDsc transform

    TExtended -> do
        let n = length names2
            Length2 n1 n2 = dLen parentDsc
            chunks = [n1] ++ repeat n2
            fx0 = B.pack [False]
            fx1 = B.pack [True]

            -- end of input
            consume (c:_) [] acc
                -- end of chunk, we are done
                | (B.length acc) == (c-1) = Just $ [acc `mappend` fx0]
                -- can not terminate in the middle of a chunk
                | otherwise = Nothing

            -- more input
            consume c'@(c:cs) input@(b:bs) acc
                -- overflow
                | (B.length acc) >= c = Nothing
                -- end of chunk
                | (B.length acc) == (c-1) = do
                    let chunk = acc `mappend` fx1
                    rest <- consume cs input mempty
                    Just $ chunk:rest
                -- not yet at the end of chunk
                | otherwise = consume c' bs (acc `mappend` b)

            consume _ _ _ = Nothing

        guard $ (length names1) >= n
        guard $ (take n names1) == names2
        l <- sequence [f val d | (val,d) <- zip values items] >>= return . map iBits
        ch <- consume chunks l mempty
        return $ Item parentDsc (mconcat ch)

    TExtendedVariant -> do
        let n = length names2
            Length2 n1 n2 = dLen parentDsc
            fx0 = B.pack [False]
            fx1 = B.pack [True]

            genPrim 1 [] = Just [fx0]
            genPrim n' bss@(b:bs)
                | n'<1 = Nothing
                | n'==1 = case bss of
                    [] -> Just [fx0]
                    _ -> do
                        sec <- genSec n2 bss
                        Just $ fx1:sec
                | otherwise = do
                    rest <- genPrim (n' - B.length b) bs
                    Just $ b:rest
            genPrim _ _ = Nothing

            genSec 0 [] = Just []
            genSec n' (b:bs)
                | n < 0 = Nothing
                | otherwise = do
                    rest <- genSec (n' - B.length b) bs
                    Just $ b:rest
            genSec _ _ = Nothing
        
        guard $ (length names1) >= n
        guard $ (take n names1) == names2
        l <- sequence [f val d | (val,d) <- zip values items] >>= return . map iBits
        ch <- genPrim n1 l
        return $ Item parentDsc (mconcat ch)

    _ -> Nothing

    where 
        items = dItems parentDsc
        names1 = map dName items
        names2 = map fst list
        values = map snd list

-- | Get bits.
toBits :: Item -> Maybe B.Bits
toBits = Just . iBits

-- | Get raw value.
toRaw :: Integral a => Item -> Maybe a
toRaw = Just . B.toUIntegral . iBits

-- | Get item's natural value.
toNatural :: Item -> Maybe EValue
toNatural item = do
    let b = iBits item
        uval = B.toUIntegral b
        sval = B.toSIntegral b

    (val,mmin,mmax) <- case (dValue . iDsc $ item) of
        VDecimal lsb _ mmin mmax -> Just (lsb*sval, mmin, mmax)
        VUnsignedDecimal lsb _ mmin mmax -> Just (lsb*uval, mmin, mmax)
        VInteger _ mmin mmax -> Just (sval, mmin, mmax)
        VUnsignedInteger _ mmin mmax -> Just (uval, mmin, mmax)
        _ -> Nothing
    
    return val >>= _chkLimit mmin (<) >>= _chkLimit mmax (>)

