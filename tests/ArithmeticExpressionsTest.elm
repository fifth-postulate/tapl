module ArithmeticExpressionsTest exposing (suite)

import ArithmeticExpressions as Expression exposing (Term(..), eval, fromInt, isNumerical, isValue, parse)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Arithmetic Expressions"
        [ describe "fromInt"
            [ test "0" <|
                \_ ->
                    let
                        actual =
                            fromInt 0

                        expected =
                            TmZero
                    in
                    Expect.equal actual expected
            , test "1" <|
                \_ ->
                    let
                        actual =
                            fromInt 1

                        expected =
                            TmSucc TmZero
                    in
                    Expect.equal actual expected
            ]
        , describe "isNumerical"
            [ test "TmZero is numerical" <|
                \_ ->
                    toBeTrue (isNumerical TmZero)
            , test "(TmSucc TmZero) is numerical" <|
                \_ ->
                    toBeTrue (isNumerical (TmSucc TmZero))
            ]
        , describe "isValue"
            [ test "TmZero is a value" <|
                \_ ->
                    toBeTrue (isValue TmZero)
            , test "TmFalse is a value" <|
                \_ ->
                    toBeTrue (isValue TmFalse)
            , test "TmTrue is a value" <|
                \_ ->
                    toBeTrue (isValue TmTrue)
            ]
        , describe "eval"
            [ test "complex expression" <|
                \_ ->
                    let
                        actual =
                            TmIf (TmIsZero (TmSucc TmZero)) (TmSucc TmZero) TmZero
                                |> eval

                        expected =
                            TmZero
                    in
                    Expect.equal actual expected
            ]
        , describe "parse"
            [ parseTest { input = "O", expected = TmZero }
            , parseTest { input = "T", expected = TmTrue }
            , parseTest { input = "F", expected = TmFalse }
            , parseTest { input = "SO", expected = TmSucc TmZero }
            , parseTest { input = "PO", expected = TmPred TmZero }
            , parseTest { input = "?SO", expected = TmIsZero (TmSucc TmZero) }
            , parseTest { input = "(?SO)", expected = TmIsZero (TmSucc TmZero) }
            , parseTest { input = "if(?SO)(SO)(O)", expected = TmIf (TmIsZero (TmSucc TmZero)) (TmSucc TmZero) TmZero }
            ]
        , describe "print"
            [ printTest { input = TmZero, expected = "O" }
            , printTest { input = TmTrue, expected = "T" }
            , printTest { input = TmFalse, expected = "F" }
            , printTest { input = TmSucc TmZero, expected = "S(O)" }
            , printTest { input = TmPred TmZero, expected = "P(O)" }
            , printTest { input = TmIsZero TmZero, expected = "?(O)" }
            , printTest { input = TmIf (TmIsZero TmZero) TmZero (TmSucc TmZero), expected = "if(?(O))(O)(S(O))" }
            ]
        ]


toBeTrue : Bool -> Expectation
toBeTrue value =
    if value then
        Expect.pass

    else
        Expect.fail "value is not true"


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
