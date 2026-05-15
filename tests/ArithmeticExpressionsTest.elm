module ArithmeticExpressionsTest exposing (suite)

import ArithmeticExpressions exposing (Term(..), eval, fromInt, isNumerical, isValue)
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
        ]


toBeTrue : Bool -> Expectation
toBeTrue value =
    if value then
        Expect.pass

    else
        Expect.fail "value is not true"
