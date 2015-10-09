----------------
-- |
-- Module       :  Data.Asterix.Expression
--
-- Maintainer   : Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module provides python like math expression evaluator
-- for some basic expressions.
--
-- Examples:
--
-- >    > eval "1+2"
-- >    Just 3.0
-- >
-- >    > eval "pow(2,10)"
-- >    Just 1024.0
-- >
-- >    > eval "-180.0/pow(0x02, 23)"
-- >    Just (-2.1457672119140625e-5)
-- >
-- >    > eval "test"
-- >    Nothing
--

module Data.Asterix.Expression
( eval
) where

import Language.Python.Version3 as P
import Language.Python.Common.AST as A

-- | Parse and evaluate string expression.
eval :: String -> Maybe Double
eval s = do
    ast <- case P.parseExpr s "" of
            Right (x,_) -> Just x
            _ -> Nothing
    eval2 ast
    where
        eval2 e@A.Int {} = Just . fromIntegral . int_value $ e
        eval2 e@A.LongInt {} = Just . fromIntegral . int_value $ e
        eval2 e@A.Float {} = Just . float_value $ e
        eval2 e@A.UnaryOp {op_arg=a} = do
            x <- eval2 a
            Just (op x)
            where
                op x = case operator e of
                    Plus {} -> x
                    Minus {} -> 0 - x
                    _ -> undefined
        eval2 e@A.BinaryOp {left_op_arg=l, right_op_arg=r} = do
            x <- (eval2 l)
            y <- (eval2 r) 
            Just (x `op` y)
            where
                op = case operator e of
                    Plus {} -> (+)
                    Minus {} -> (-)
                    Multiply {} -> (*)
                    Divide {} -> (/)
                    _ -> undefined
        eval2 A.Paren {paren_expr=expr} = eval2 expr
        eval2 e@A.Call {} = do
            let func = ident_string . var_ident . call_fun $ e
                args' = map (eval2 . arg_expr) (call_args e)
            args <- sequence args'
            case func of
                "pow" -> case (length args) of
                            2 -> Just $ args!!0 ^^ (truncate (args!!1) :: Integer)
                            _ -> Nothing
                _ -> Nothing
        eval2 _ = Nothing

