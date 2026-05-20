module ArithmeticExpressionsTest exposing (suite)

import ArithmeticExpressions as Expression exposing (Term(..), eval, isNumerical, isValue, parse)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Arithmetic Expressions"
        [ describe "fromInt"
            [ fromIntTest { input = 0, expected = TmZero }
            , fromIntTest { input = 1, expected = TmSucc TmZero }
            , fromIntTest { input = -1, expected = TmZero }
            , fromIntTest { input = 3, expected = TmSucc (TmSucc (TmSucc TmZero)) }
            ]
        , describe "isNumerical"
            [ isNumericalTest { input = TmZero }
            , isNumericalTest { input = TmSucc TmZero }
            ]
        , describe "isValue"
            [ isValueTest { input = TmZero }
            , isValueTest { input = TmFalse }
            , isValueTest { input = TmTrue }
            , isValueTest { input = TmSucc TmZero }
            ]
        , describe "eval"
            [ evalTest { input = TmZero, expected = TmZero }
            , evalTest { input = TmIf (TmIsZero TmZero) TmFalse TmTrue, expected = TmFalse }
            ]
        , describe "parse"
            [ parseTest { input = "O", expected = TmZero }
            , parseTest { input = "true", expected = TmTrue }
            , parseTest { input = "false", expected = TmFalse }
            , parseTest { input = "succ(O)", expected = TmSucc TmZero }
            , parseTest { input = "succ O", expected = TmSucc TmZero }
            , parseTest { input = "pred(O)", expected = TmPred TmZero }
            , parseTest { input = "pred O", expected = TmPred TmZero }
            , parseTest { input = "iszero(succ O)", expected = TmIsZero (TmSucc TmZero) }
            , parseTest { input = "(iszero(succ O))", expected = TmIsZero (TmSucc TmZero) }
            , parseTest { input = "if iszero succ(O) then succ O else O", expected = TmIf (TmIsZero (TmSucc TmZero)) (TmSucc TmZero) TmZero }
            ]
        , describe "print"
            [ printTest { input = TmZero, expected = "O" }
            , printTest { input = TmTrue, expected = "true" }
            , printTest { input = TmFalse, expected = "false" }
            , printTest { input = TmSucc TmZero, expected = "succ(O)" }
            , printTest { input = TmPred TmZero, expected = "pred(O)" }
            , printTest { input = TmIsZero TmZero, expected = "iszero(O)" }
            , printTest { input = TmIf (TmIsZero TmZero) TmZero (TmSucc TmZero), expected = "if(iszero(O))then(O)else(succ(O))" }
            ]
        ]


type alias FromIntTestCase =
    { input : Int
    , expected : Term
    }


fromIntTest : FromIntTestCase -> Test
fromIntTest testCase =
    test (String.fromInt testCase.input) <|
        \_ ->
            let
                actual =
                    Expression.fromInt testCase.input
            in
            Expect.equal actual testCase.expected


toBeTrue : Bool -> Expectation
toBeTrue value =
    if value then
        Expect.pass

    else
        Expect.fail "value is not true"


type alias NumericalTestCase =
    { input : Term
    }


isNumericalTest : ValueTestCase -> Test
isNumericalTest testCase =
    test (Expression.toString testCase.input) <|
        \_ ->
            toBeTrue (isNumerical testCase.input)


type alias ValueTestCase =
    { input : Term
    }


isValueTest : ValueTestCase -> Test
isValueTest testCase =
    test (Expression.toString testCase.input) <|
        \_ ->
            toBeTrue (isValue testCase.input)


type alias EvalTestCase =
    { input : Term
    , expected : Term
    }


evalTest : EvalTestCase -> Test
evalTest testCase =
    test (Expression.toString testCase.input) <|
        \_ ->
            let
                actual =
                    testCase.input
                        |> eval
            in
            Expect.equal actual testCase.expected


type alias ParseTestCase =
    { input : String
    , expected : Term
    }


parseTest : ParseTestCase -> Test
parseTest testCase =
    test testCase.input <|
        \_ ->
            let
                actual =
                    parse testCase.input
            in
            Expect.equal actual (Just testCase.expected)


type alias PrintTestCase =
    { input : Term
    , expected : String
    }


printTest : PrintTestCase -> Test
printTest testCase =
    test testCase.expected <|
        \_ ->
            let
                actual =
                    Expression.toString testCase.input
            in
            Expect.equal actual testCase.expected
