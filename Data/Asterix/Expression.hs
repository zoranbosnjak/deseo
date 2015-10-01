
{-
python like math expression evaluator

Author: Zoran Bosnjak (Sloveniacontrol)
-}

module Expression
( eval
) where

import Language.Python.Version3 as P
import Language.Python.Common.AST as A
import Test.QuickCheck

-- parse and evaluate string expression
eval s = do
    ast <- case P.parseExpr s "" of
            Right (span,tok) -> Just span
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
        eval2 e@A.Paren {paren_expr=exp} = eval2 exp
        eval2 e@A.Call {} = do
            let func = ident_string . var_ident . call_fun $ e
                args' = map (eval2 . arg_expr) (call_args e)
            args <- sequence args'
            case func of
                "pow" -> Just $ args!!0 ^^ (truncate (args!!1))
                _ -> Nothing
        eval2 _ = Nothing

main :: IO ()
main = do

    putStrLn "quick check test run..."

    -- TODO: make explicit loop over all elements in a list (not N passes)
    quickCheck testAll where
        testAll :: Property
        testAll = forAll (elements testcase) $ \(x,y) ->
            let (Just out) = eval x
            in out == y
            where
                testcase =
                    [ ("1", 1)
                    , ("(1)", 1)
                    , ("1+2", 3)
                    , ("1 + 2 * 3 + 4", 11)
                    , ("-1+2", 1)
                    , ("-5-10", -15)
                    , ("2*3", 6)
                    , ("1/2", 0.5)
                    , ("pow(2,10)", 1024)
                    , ("-180.0/pow(0x02, 23)", -2.1457672119140625e-05)
                    ]

