----------------
-- |
-- Module:      Data.Asterix.Expression
-- Copyright:   (c) 2015-2016 Zoran Bošnjak
--              (c) 2015-2016 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides python like math expression evaluator
-- for some basic expressions.
--

{-# LANGUAGE DeriveGeneric #-}

module Data.Asterix.Expression
( EValue(EInteger, EDouble)
, eval
) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (mzero, void)
import GHC.Generics (Generic)
import Text.Megaparsec (between, parseMaybe, try, Parsec)
import Text.Megaparsec.Char (spaceChar)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL))
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data EValue
    = EInteger Integer
    | EDouble Double
    deriving (Show, Generic)

instance NFData EValue

-- | Convert to Double
_toDouble :: EValue -> Double
_toDouble (EInteger x) = fromIntegral x
_toDouble (EDouble x) = x

instance Num EValue where

    -- (+)
    EInteger a + EInteger b = EInteger $ a+b
    a + b = EDouble $ (_toDouble a)+(_toDouble b)

    -- (*)
    EInteger a * EInteger b = EInteger $ a*b
    a * b = EDouble $ (_toDouble a)*(_toDouble b)

    -- abs
    abs (EInteger x) = EInteger $ abs x
    abs (EDouble x) = EDouble $ abs x

    -- signum
    signum (EInteger x) = EInteger $ signum x
    signum (EDouble x) = EDouble $ signum x

    -- fromInteger
    fromInteger = EInteger

    -- negate
    negate (EInteger x) = EInteger $ negate x
    negate (EDouble x) = EDouble $ negate x

instance Fractional EValue where
    fromRational = EDouble . fromRational
    recip (EInteger x) = EDouble . recip $ fromIntegral x
    recip (EDouble x) = EDouble $ recip x

instance Eq EValue where
    EInteger val1 == EInteger val2 = val1 == val2
    val1 == val2 = _toDouble val1 == _toDouble val2

instance Ord EValue where
    compare (EInteger val1) (EInteger val2) = compare val1 val2
    compare val1 val2 = compare (_toDouble val1) (_toDouble val2)


data Expr
  = EConst EValue
  | EPow Integer Integer
  | EAdd Expr Expr
  | ESubtract Expr Expr
  | EMultiply Expr Expr
  | EDivide Expr Expr
  deriving (Show)

-- sc stands for "space consumer" as per Megaparsec convention
sc :: Parser ()
sc = L.space (void spaceChar) mzero mzero

symbol :: String -> Parser String
symbol = L.symbol sc

parened :: Parser a -> Parser a
parened = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = L.signed sc $ hex <|> L.decimal where
    hex = (try $ symbol "0x") >> L.hexadecimal

double :: Parser Double
double = L.signed sc L.float

evalue :: Parser EValue
evalue = try (EDouble <$> double) <|> (EInteger <$> integer)

pow :: Parser Expr
pow = do
    void $ symbol "pow"
    parened $ do
        a <- integer
        void $ symbol ","
        b <- integer
        return $ EPow a b

binOp :: Parser Expr
binOp = makeExprParser term
  [ [ InfixL (EMultiply <$ symbol "*")
    , InfixL (EDivide   <$ symbol "/") ]
  , [ InfixL (EAdd      <$ symbol "+")
    , InfixL (ESubtract <$ symbol "-") ]
  ]

term :: Parser Expr
term = parened expr <|> pow <|> (EConst <$> evalue)

expr :: Parser Expr
expr = binOp <|> term


-- | Parse and evaluate string expression.
-- Examples:
--
-- >    > eval "1+2"
-- >    Just (EInteger 3)
-- >
-- >    > eval "pow(2,10)"
-- >    Just (EInteger 1024)
-- >
-- >    > eval "-180.0/pow(0x02, 23)"
-- >    Just (EDouble (-2.1457672119140625e-5))
--
-- >    eval "pow(10,-2)"
-- >    Just (EDouble 1.0e-2)
-- >
-- >    > eval "test"
-- >    Nothing
--
eval :: String -> Maybe EValue
eval s = compute <$> parseMaybe expr s

compute :: Expr -> EValue
compute (EConst e) = e
compute (EPow m e) = if (e>=0)
                     then EInteger $ m^e
                     else EDouble $ 1.0 / (fromInteger (m^(-e)))
compute (EAdd a b) = compute a + compute b
compute (ESubtract a b) = compute a - compute b
compute (EMultiply a b) = compute a * compute b
compute (EDivide a b) = compute a / compute b

