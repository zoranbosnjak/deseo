{-# LANGUAGE DeriveGeneric #-}

----------------
-- |
-- Module       :  Data.Asterix.Expression
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides python like math expression evaluator
-- for some basic expressions.
--

module Data.Asterix.Expression
(   EValue(..)
    , eval
) where

import Control.DeepSeq.Generics
import GHC.Generics
import Language.Python.Version3 as P
import Language.Python.Common.AST as A

data EValue =
    EInteger Integer
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
eval s = do
    ast <- case P.parseExpr s "" of
            Right (x,_) -> Just x
            _ -> Nothing
    eval2 ast
    where

        eval2 e@A.Int {} = Just . EInteger . int_value $ e

        eval2 e@A.LongInt {} = Just . EInteger . int_value $ e
        
        eval2 e@A.Float {} = Just . EDouble . float_value $ e
        
        eval2 e@A.UnaryOp {op_arg=a} = do
            x <- eval2 a
            op <- case (operator e) of
                    Plus {} -> Just id
                    Minus {} -> Just negate
                    _ -> Nothing
            Just (op x)

        eval2 e@A.BinaryOp {left_op_arg=l, right_op_arg=r} = do
            x <- (eval2 l)
            y <- (eval2 r) 
            op <- case operator e of
                    Plus {} -> Just (+)
                    Minus {} -> Just (-)
                    Multiply {} -> Just (*)
                    Divide {} -> Just (/)
                    _ -> Nothing
            Just (x `op` y)

        eval2 A.Paren {paren_expr=expr} = eval2 expr

        eval2 e@A.Call {} = do
            let func = ident_string . var_ident . call_fun $ e
                args' = map (eval2 . arg_expr) (call_args e)
            args <- sequence args'
            case func of
                "pow" -> case args of
                            [EInteger a, EInteger b] -> if (b>=0)
                                                then Just . EInteger $ a^b
                                                else Just . EDouble $ 1.0 / (fromInteger (a^(-b)))
                            _ -> Nothing
                _ -> Nothing

        eval2 _ = Nothing

