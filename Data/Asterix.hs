----------------
-- |
-- Module:      Data.Asterix
-- Copyright:   (c) 2015-2016 Zoran Bošnjak
--              (c) 2015-2016 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
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
--
--      * decode bits to records
--
-- >        import qualified Data.BitString as B
-- >
-- >        let db = B.fromInteger 48 0x000006800203
-- >
-- >        profiles <- ...
-- >        let parse db = return db
-- >                >>= toDataBlocks
-- >                >>= mapM (toRecords profiles)
-- >                >>= return . join
-- >
-- >        print $ parse db
--
--      * decode items
--
-- >        rec <- fetchRecord...
-- >        let sac = childR ["010", "SAC"] rec >>= toRaw
-- >            sic = childR ["010", "SIC"] rec >>= toRaw
--
--      * update records
--
-- >        -- rewrite item with constant, delete item
-- >        rec <- fetchRecord...
-- >        let rec' = update rec $ do
-- >                modifyItem "010" $ \_ -> fromRawInt 0x0103
-- >                delItem "020"
-- >
-- >        -- swap SAC and SIC values (method 1)
-- >        rec <- fetchRecord...
-- >        Just b = update rec $ do
-- >            let Just sac = childR ["010", "SAC"] rec >>= toRaw
-- >                Just sic = childR ["010", "SIC"] rec >>= toRaw
-- >            modifyItemR ["010", "SAC"] $ \_ -> fromRawInt sic
-- >            modifyItemR ["010", "SIC"] $ \_ -> fromRawInt sac
-- >
-- >        -- swap SAC and SIC values (method 2)
-- >        rec <- fetchRecord...
-- >        Just c = update rec $ do
-- >            modifyItem "010" $ \i dsc -> do
-- >                sac <- B.take 8 <$> toBits i
-- >                sic <- B.take 8 . B.drop 8 <$> toBits i
-- >                fromBits (sic <> sac) dsc
--
--      * building items
--
-- >        catXY <- ...
-- >        rec <- create catXY $ do
-- >            "010" <! fromBits (B.fromInteger 16 0x0102)
-- >            "020" <! fromRawInt 0x0203
-- >            "030" <! fromValues fromRawInt [("SAC", 0x01), ("SIC", 0x02)]
-- >            "030a" <! fromSubitems   -- the same as 030, but more flexible
-- >                [ ("SAC", fromRawInt 0x01)  -- allow different
-- >                , ("SIC", fromRawInt 0x02)  -- item convertors
-- >                ]
-- >            "040" <!! do                    -- compound item
-- >                "A" <! fromRawInt 0x01
-- >                "B" <! fromRawInt 0x02
-- >            "050" <! fromString "STRING"
-- >
-- >        rec <- fromValues fromRawInt [("010", 0x0102)] catXY
-- >
-- >        rec <- fromRepetitiveValues (fromValues fromRawInt)
-- >            [ [("A", 1), ("B", 2)], [("A", 3), ("B", 4)] ] catXY
--
--

{-# LANGUAGE DeriveGeneric #-}

module Data.Asterix
(
    -- * Data types
    ItemType(..)
    , Item(..), emptyItem
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
    , mkDataBlock
    , fromDataBlock

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
    , update, create, (<!), putItem, delItem, failItem, getItem
    , createSubitem, (<!!)
    , modifyItem, modifyItemR

    -- * Converters: value -> Maybe Item
    , fromBits
    , fromRaw
    , fromRawInt
    , fromRawInteger
    , fromNatural
    , fromString
    , fromSubitems
    , fromValues
    , fromRepetitiveValues

    -- * Converters: Item -> Maybe value
    , toBits
    , toRaw
    , toNatural
    , toString

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
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as BS8

import Control.DeepSeq (NFData)
import Control.Monad (forM, guard, join)
import Control.Monad.State (State, state, execState, gets, modify)
import GHC.Generics (Generic)

import qualified Text.XML.Light as X
import Text.XML.Light.Lexer (XmlSource)

import qualified Data.BitString as B
import Data.Asterix.Expression (EValue(EInteger, EDouble), eval)

-- | Asterix item types
data ItemType
    = TItem
    | TFixed
    | TSpare
    | TExtended
    | TExtendedVariant
    | TRepetitive
    | TExplicit
    | TCompound
    | TRfs
    deriving (Show, Read, Eq, Generic)

instance NFData ItemType

-- | description + data
data Item = Item
    { iDsc  :: Desc
    , iBits :: B.Bits
    } deriving (Generic, Show, Eq)

instance NFData Item

emptyItem :: Desc -> Item
emptyItem dsc = Item dsc mempty

-- | Asterix item description
data Desc = Desc
    { dName     :: ItemName
    , dItemType :: ItemType
    , dDsc      :: ItemDescription
    , dLen      :: Length
    , dItems    :: [Desc]
    , dValue    :: Value
    } deriving (Eq, Generic)

instance NFData Desc

instance Show Desc where
    show d
        = (show . dItemType $ d) ++ " (" ++ (dName d) ++ "), "
        ++ (show . dLen $ d) ++ ", " ++ (show $ dValue d)

-- | Empty description
noDesc :: Desc
noDesc = Desc
    { dName = ""
    , dItemType = TItem
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
data Category = Category
    { cCat      :: Cat
    , cEdition  :: Edition
    , cUaps     :: [Uap]
    } deriving (Generic)

instance NFData Category

instance Show Category where
    show c = "(category " ++ (show $ cCat c)
        ++ ", edition " ++ (show $ cEdition c) ++ ")"

type Profiles = Map.Map Cat Category

-- | Asterix edition
data Edition = Edition Major Minor deriving (Eq, Generic)

instance NFData Edition

instance Ord Edition where
    compare (Edition a1 b1) (Edition a2 b2)
        = (compare a1 a2) `mappend` (compare b1 b2)

instance Show Edition where
    show (Edition a b) = show a ++ "." ++ show b

instance Read Edition where
    readsPrec _ value = [(Edition (read a) (read b), "")] where
        a = takeWhile (/='.') value
        b = tail . dropWhile (/='.') $ value

-- | Length of asterix item
data Length = Length0 | Length1 Int | Length2 Int Int
    deriving (Show, Read, Eq, Generic)

instance NFData Length

type Lsb = EValue
type Unit = String
type Min = EValue
type Max = EValue

data Value
    = VRaw
    | VString
    | VDecimal Lsb (Maybe Unit) (Maybe Min) (Maybe Max)
    | VUnsignedDecimal Lsb (Maybe Unit) (Maybe Min) (Maybe Max)
    | VInteger (Maybe Unit) (Maybe Min) (Maybe Max)
    | VUnsignedInteger (Maybe Unit) (Maybe Min) (Maybe Max)
    deriving (Eq, Show, Generic)

instance NFData Value

data DataBlock = DataBlock
    { dbCat     :: Cat
    , dbData    :: B.Bits
    } deriving (Generic, Eq, Show)

instance NFData DataBlock

-- | Create datablock (from records).
mkDataBlock :: Cat -> [Item] -> DataBlock
mkDataBlock cat items = DataBlock {dbCat=cat, dbData=bs} where
    bs = mconcat $ map iBits items

-- | Convert datablock to bits.
fromDataBlock :: DataBlock -> B.Bits
fromDataBlock db = c <> ln <> bs where
    bs = dbData db
    c = B.fromIntegral 8 (dbCat db)
    ln = B.fromIntegral 16 $ (B.length bs `div` 8) + 3

-- | Request particular edition of a category or latest
categorySelect :: [Category] -> [(Cat,Edition)] -> Either String Profiles
categorySelect dsc requested = do
    dsc' <- categorySelectAll dsc
    fmap Map.fromList $ forM (Map.toList dsc') $ \(cat,editions) -> do
        case lookup cat requested of
            Nothing -> Right $ (cat, last editions)
            Just ed -> case lookup ed [(cEdition c, c) | c <- editions] of
                Nothing -> Left $ "Cat " ++ (show cat)
                    ++ ", edition " ++ (show ed) ++ " not found!"
                Just val -> Right (cat, val)

-- | Group descriptions by category and sort by edition
categorySelectAll :: [Category] -> Either String (Map.Map Cat [Category])
categorySelectAll dsc =
    let cats = nub . map cCat $ dsc
    in  fmap Map.fromList $ forM cats $ \cat ->
    let editions =
            sortBy (compare `on` cEdition) . filter ((==cat) . cCat) $ dsc
        eds = map cEdition editions
    in case nub eds == eds of
        True -> Right $ (cat, editions)
        False -> Left $ "duplicated editions in cat: " ++ show cat

-- | Read xml content.
categoryDescription :: XmlSource s => s -> Either String Category
categoryDescription src = do
    -- TODO break into smaller pieces
    let elements = X.onlyElems . X.parseXML $ src
        filteredElements =
            filter (\e -> (X.qName . X.elName $ e) == "category") $ elements

    category <- case filteredElements of
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
            uapItems <- forM (map X.strContent . X.elChildren $ uap) $ \item ->
                case item of
                    "" -> Right noDesc
                    _ -> case (lookup item items) of
                        Nothing -> Left $ "item " ++ (show item)
                            ++ " not found in <items>"
                        Just x -> Right x
            let uapName = X.qName . X.elName $ uap
                topLevel = Desc
                    { dName = ""
                    , dItemType = TCompound
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
        Nothing -> Left $
            "Attribute '"++ aName ++ "' not found in element " ++ (show el)
        Just x -> Right $ read x

    getChild el aName = case X.findChild (nameOf aName) el of
        Nothing -> Left $
            "Child '"++ aName ++ "' not found in element " ++ (show el)
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
    recalculateLen dsc = fromMaybe Length0 (total dsc >>= Just . Length1)
      where
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
        f dsc@Desc {dItemType=TItem, dLen=Length0} = Right $
            dsc {dLen=recalculateLen dsc}
        f dsc@Desc {dItemType=TFixed, dLen=Length1 _, dItems=[]} = Right dsc
        f dsc@Desc {dItemType=TSpare, dLen=Length1 _, dItems=[]} = Right dsc
        f dsc@Desc {dItemType=TExtended, dLen=Length2 _ _} = Right dsc
        f dsc@Desc {dItemType=TExtendedVariant, dLen=Length2 _ _} = Right dsc
        f dsc@Desc {dItemType=TRepetitive, dLen=Length0, dItems=(_:_)} = Right $
            dsc {dLen=recalculateLen dsc}
        f dsc@Desc {dItemType=TExplicit, dLen=Length0, dItems=[]} = Right dsc
        f dsc@Desc {dItemType=TCompound, dLen=Length0, dItems=(_:_)} = Right dsc
        f dsc@Desc {dItemType=TRfs, dLen=Length0, dItems=[]} = Right dsc
        f x = Left $ "error in description: " ++ (dName x)

        -- get all elements
        dsc' e = do
            name <- case X.findAttr (nameOf "name") e of
                Nothing -> Right ""
                Just n -> Right n
            itemType <- return . read . ("T"++) . fromMaybe "Item"
                . X.findAttr (nameOf "type") $ e
            dsc <- return $ either (\_->"") (X.strContent)
                (getChild e "dsc")
            len <- return (either (\_->"") (X.strContent)
                (getChild e "len")) >>= readLength
            items <- sequence $ map readItem $
                either (\_->[]) X.elChildren (getChild e "items")
            val <- getValueTip e

            Right $ Desc
                { dName = name
                , dItemType = itemType
                , dDsc = dsc
                , dLen = len
                , dItems = items
                , dValue = val
                }

        getValueTip :: X.Element -> Either String Value
        getValueTip el' = case getChild el' "convert" of
            Left _ -> Right VRaw
            Right conv -> do
                itemType <- getChild conv "type" >>= return . X.strContent

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

                case itemType of
                    "string" -> Right VString
                    "decimal" -> case lsb of
                        Nothing -> Left $ "'lsb' missing " ++ (show el')
                        Just lsb' -> Right $ VDecimal lsb' unit min' max'
                    "unsigned decimal" -> case lsb of
                        Nothing -> Left $ "'lsb' missing " ++ (show el')
                        Just lsb' ->
                            Right $ VUnsignedDecimal lsb' unit min' max'
                    "integer" -> Right $ VInteger unit min' max'
                    "unsigned integer" ->
                        Right $ VUnsignedInteger unit min' max'
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
        len <- return x >>= B.dropMaybe 8 >>= B.takeMaybe 16
            >>= return . B.toUIntegral
        y <- return x >>= B.takeMaybe (len*8) >>= B.dropMaybe 24

        let db = DataBlock cat y
        rest <- return x >>= B.dropMaybe (len*8) >>= toDataBlocks
        Just (db:rest)

-- | Split datablock to records.
toRecords :: Profiles -> DataBlock -> Maybe [Item]
toRecords profiles db = do
    category <- Map.lookup cat profiles
    getRecords category $ dbData db
  where
    cat = dbCat db
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
sizeOf Desc {dItemType=TItem, dLen=Length1 n} b = checkSize n b
sizeOf Desc {dItemType=TItem, dItems=[]} _ = Just 0
sizeOf d@Desc {dItemType=TItem, dItems=(i:is)} b = do
    size <- sizeOf i b
    rest <- sizeOf (d {dItems=is}) (B.drop size b)
    Just (size+rest)

-- size of Fixed
sizeOf Desc {dItemType=TFixed, dLen=Length1 n} b = checkSize n b

-- size of Spare
sizeOf Desc {dItemType=TSpare, dLen=Length1 n} b = do
    size <- checkSize n b
    guard $ B.take size b == B.zeros size
    return size

-- size of Extended
sizeOf Desc {dItemType=TExtended, dLen=Length2 n1 n2} b = do
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
sizeOf Desc {dItemType=TExtendedVariant, dLen=Length2 n1 n2} b = do
    size <- checkSize n1 b
    case (B.index b (size-1)) of
        False -> Just size
        True -> checkSize (n1+n2) b

-- size of Repetitive
sizeOf d@Desc {dItemType=TRepetitive} b = do
    s8 <- checkSize 8 b
    let rep = B.toUIntegral . B.take s8 $ b
        b' = B.drop s8 b
    getSubitems rep b' 8
  where
    getSubitems :: Int -> B.Bits -> Size -> Maybe Size
    getSubitems 0 _ size = Just size
    getSubitems n b'' size = do
        itemSize <- sizeOf (d {dItemType=TItem}) b''
        getSubitems (n-1) (B.drop itemSize b'') (itemSize+size)

-- size of Explicit
sizeOf Desc {dItemType=TExplicit} b = do
    s8 <- checkSize 8 b
    let val = B.toUIntegral . B.take s8 $ b
    case val of
        0 -> Nothing
        _ -> checkSize (8*val) b

-- size of Compound
sizeOf Desc {dItemType=TCompound, dItems=items} b' = do
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
child name item = join $ childs item >>= lookup name

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
    | itemType == TItem = do
        let consume [] _ = Just []
            consume (i:is) b' = do
                (x,y) <- grab i b'
                rest <- consume is y
                Just $ (dName i, Just $ Item i x):rest
        consume items b

    -- Extended
    | itemType == TExtended = do
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
    | itemType == TExtendedVariant = do
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
    | itemType == TRepetitive = do
        n <- checkSize 8 b
        let dsc = iDsc item
            subDsc = dsc {dItemType = TItem}
            rep = B.toUIntegral . B.take n $ b
            dataBits = B.drop n b

            consume _ 0 _ = Just []
            consume ix remainingN remainingBits = do
                (x,y) <- grab subDsc remainingBits
                let subitem = Item {iDsc=subDsc, iBits=x}
                rest <- consume (ix+1) (remainingN-1) y
                Just $ (show ix, Just subitem):rest

        consume (0::Int) (rep::Int) dataBits

    -- Compound
    | itemType == TCompound = do
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
    itemType = dItemType . iDsc $ item
    items = dItems . iDsc $ item
    b = iBits item

-- | Recreate item from subitems.
unChilds :: Desc -> [(ItemName,Maybe Item)] -> Maybe Item
unChilds dsc present = case (dItemType dsc) of
    TItem -> do
        guard $ length items == length present
        return $ Item dsc $ mconcat . map iBits . catMaybes . map snd $ present

    -- TFixed ->

    -- TSpare ->

    TExtended -> do
        bs <- compose mempty $ zip items present
        return $ Item dsc bs
      where
        compose acc [] = case B.length acc `mod` 8 of
            7 -> return $ acc <> B.pack [False] -- terminate record
            _ -> Nothing
        compose acc ((d, (name, mi)):rest) = case mi of
            Nothing -> compose acc rest
            Just i -> do
                guard $ name == dName d
                guard $ d == iDsc i
                let acc' = case B.length acc `mod` 8 of
                        7 -> acc <> B.pack [True] -- extend record
                        _ -> acc
                compose (acc' <> iBits i) rest

    TRepetitive -> do
        let n = length present
        guard $ n < 256
        return $ Item dsc $ (B.fromIntegral 8 n) <> bs
      where
        bs = mconcat . map iBits . catMaybes . map snd $ present

    -- TExplicit ->

    TCompound -> do
        guard $ length items == length present
        return $ Item dsc bs
      where
        bs = B.pack fspecTotal
            `mappend` (mconcat . map iBits . catMaybes . map snd $ present)
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
    _ -> Nothing
  where
    items = dItems dsc

-- | Update item.
update :: Item -> State (Maybe Item) () -> Maybe Item
update i act = execState act (Just i)

-- | Create compound item from profile and transform function.
create :: Desc -> State (Maybe Item) () -> Maybe Item
create dsc transform
    | dItemType dsc == TCompound = update (emptyItem dsc) transform
    | otherwise = Nothing
    -- TODO: try to replace with: MaybeT (State Item) ->

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
putItem name toItem = modify (>>= newItem) where
    newItem parent = case dItemType $ iDsc $ parent of
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

-- | Alias for 'createSubitem'
--
-- >
-- >    rec <- create cat $ do
-- >        "010" <! fromRaw 0x0102
-- >        "020" <!! do
-- >            "A" <! fromRaw 0x01
-- >            "B" <! fromRaw 0x02
--
(<!!) :: String -> State (Maybe Item) () -> State (Maybe Item) ()
(<!!) = createSubitem

-- | Create nested compound item.
createSubitem :: String -> State (Maybe Item) () -> State (Maybe Item) ()
createSubitem name act = modify (>>= newItem) where
    newItem parent = do
        dsc <- lookup name [(dName d, d) | d <- dItems . iDsc $ parent]
        sub <- create dsc act
        execState (putItem name (\_ -> Just sub)) (Just parent)

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
delItem name = modify (>>= newItem) where
    newItem parent = case dItemType $ iDsc $ parent of
        TCompound -> childs parent
            >>= return . fmap remove
            >>= unChilds (iDsc parent)
        _ -> Nothing
    remove (a, b)
        | a == name = (a, Nothing)
        | otherwise = (a, b)

-- | Get subitem.
getItem :: ItemName -> State (Maybe Item) (Maybe Item)
getItem name = gets (>>= child name)

-- | Modify subitem (if exists).
modifyItem :: ItemName -> (Item -> Desc -> Maybe Item) -> State (Maybe Item) ()
modifyItem name f = modify (>>= newItem) where
    newItem parent = do
        subitems <- childs parent
        subitem <- join $ lookup name subitems
        let replaceItem (n,x)
                | n == name = (n, f subitem (iDsc subitem))
                | otherwise = (n, x)
        unChilds (iDsc parent) (fmap replaceItem subitems)

-- | Modify recursive subitem (if exists).
modifyItemR :: [ItemName] -> (Item -> Desc -> Maybe Item) -> State (Maybe Item) ()
modifyItemR [] _f = return ()
modifyItemR (name:[]) f = modifyItem name f
modifyItemR (name:names) f = do
    item <- execState (modifyItemR names f) <$> getItem name
    putItem name $ const item

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
fromBits val dsc = case (dItemType dsc) of
    TExplicit -> do
        let (a,b) = divMod (B.length val) 8
            ln = a+1
            size = B.fromIntegral 8 ln
        guard (b==0)
        guard (ln<=255)
        return $ Item dsc (size `mappend` val)
    _ -> undefined -- TODO: take value, decode, encode, check

-- | Convert from raw value (item size must be known).
fromRaw :: Integral a => a -> Desc -> Maybe Item
fromRaw val dsc@Desc {dLen=Length1 len} =
    Just $ Item dsc $ B.fromIntegral len val
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
    val' <- return val >>= _chkLimit mmin (<) >>= _chkLimit mmax (>)
        >>= return . ival . conv
    return $ Item dsc (B.fromIntegral len val')
fromNatural _ _ = Nothing

-- | Convert from ascii string.
fromString :: String -> Desc -> Maybe Item
fromString s dsc@Desc {dLen=Length1 len} = do
    guard $ len == (8 * length s)
    return $ Item dsc (B.fromByteString $ BS8.pack s)
fromString _ _ = Nothing

-- | Convert from given subitems
fromSubitems :: [(ItemName, Desc -> Maybe Item)] -> Desc -> Maybe Item
fromSubitems lst parentDsc = case (dItemType parentDsc) of

    TItem -> do
        guard $ names1 == names2
        l <- sequence [val d | (val,d) <- zip values items]
        return $ Item parentDsc (mconcat . map iBits $ l)

    TCompound -> do
        let transform = sequence_ [putItem name val | (name,val) <- lst]
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
        l <- traverse (fmap iBits) [val d | (val,d) <- zip values items]
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
        l <- sequence [val d | (val,d) <- zip values items]
            >>= return . map iBits
        ch <- genPrim n1 l
        return $ Item parentDsc (mconcat ch)

    _ -> Nothing

  where
    items = dItems parentDsc
    names1 = map dName items
    names2 = map fst lst
    values = map snd lst

-- | Convert from given values, convert each, then apply
fromValues :: (a -> Desc -> Maybe Item) -> [(ItemName, a)] -> Desc -> Maybe Item
fromValues f lst = fromSubitems [(name, f val) | (name, val) <- lst]

-- | Convert repetitive values
fromRepetitiveValues :: (t -> Desc -> Maybe Item) -> [t] -> Desc -> Maybe Item
fromRepetitiveValues f lst parentDsc = case (dItemType parentDsc) of

    TRepetitive -> do
        guard $ (length lst) <= 255
        let n = B.fromIntegral 8 $ length lst
            -- itemType is TRepetitive, but individual items are TItem
            dsc = parentDsc {dItemType=TItem}
        items <- sequence [f val dsc | val <- lst] >>= return . map iBits
        return $ Item parentDsc $ mconcat (n:items)

    _ -> Nothing

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

-- | Get item as ascii string.
toString :: Item -> Maybe String
toString item = B.checkAligned (iBits item) >>= return . BS8.unpack . B.toByteString

