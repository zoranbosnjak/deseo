----------------
-- |
-- Module       :  Data.Asterix
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides encoder/decoder for Asterix data.
--
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Data.Asterix as A
--
-- Examples:
--
--      * parse XML
--  
-- >        s <- readFile "path/to/catXY.xml"
-- >        let c = A.categoryDescription s
--
--      * TODO (add other examples)
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

    -- * XML parsers
    , categoryDescription

    -- * Expression evaluation
    , eval

) where

import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.Word

import Control.DeepSeq.Generics
import Control.Monad.State

import qualified Text.XML.Light as X
import Text.XML.Light.Lexer (XmlSource)

import qualified Data.BitString as B
import Data.Asterix.Expression (eval)

-- | Asterix item types
data Tip = TItem
           | TFixed
           | TSpare
           | TExtended
           | TRepetitive
           | TExplicit
           | TCompound
           -- TRfs
           deriving (Show, Read, Eq)

-- | description + data
data Item = Item Desc B.Bits deriving (Show,Eq)

-- | Asterix item description
data Desc = Desc {  dName       :: ItemName
                    , dTip      :: Tip
                    , dDsc      :: ItemDescription
                    , dLen      :: Length
                    , dItems    :: [Desc]
                    , dValue    :: Value
                 } deriving (Eq)
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
        ,cEd :: Edition 
        ,cUaps :: [Uap]
    }
instance NFData Category
instance Show Category where
    show c = "(category " ++ (show $ cCat c) ++ ", edition " ++ (show $ cEd c) ++ ")"

-- | Asterix edition
data Edition = Edition Major Minor deriving (Eq)
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

    Right $ Category {cCat=cat, cEd=ed, cUaps=dscr} 
    
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
            f dsc@Desc {dTip=TItem, dLen=Length0, dItems=(_:_)} = Right $ dsc {dLen=recalculateLen dsc}
            f dsc@Desc {dTip=TFixed, dLen=Length1 _, dItems=[]} = Right dsc
            f dsc@Desc {dTip=TSpare, dLen=Length1 _, dItems=[]} = Right dsc
            f dsc@Desc {dTip=TExtended, dLen=Length2 _ _} = Right dsc
            f dsc@Desc {dTip=TRepetitive, dLen=Length0, dItems=(_:_)} = Right $ dsc {dLen=recalculateLen dsc}
            f dsc@Desc {dTip=TExplicit, dLen=Length0, dItems=[]} = Right dsc
            f dsc@Desc {dTip=TCompound, dLen=Length0, dItems=(_:_)} = Right dsc
            f x = Left $ "error in description: " ++ (dName x)

            -- get all elements
            dsc' e = do
                name <- case X.findAttr (nameOf "name") e of
                    Nothing -> Left $ "name not found: " ++ (show e)
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

