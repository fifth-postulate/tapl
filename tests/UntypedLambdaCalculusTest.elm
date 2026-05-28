module UntypedLambdaCalculusTest exposing (suite)

import Expect exposing (Expectation)
import General
import Test exposing (..)
import UntypedLambdaCalculus as Expression exposing (Term(..), empty, isValue, parse, step)


suite : Test
suite =
    describe "Untyped Lambda Calculus"
        [ describe "isValue"
            [ isValueTest { input = TmAbs "x" (TmVar 0 1) }
            ]
        , describe "eval"
            [ evalTest { input = TmVar 0 1, expected = TmVar 0 1 }
            ]
        , describe "parse"
            [ parseTest { input = "x", expected = TmVar 0 1 }
            ]
        , describe "print"
            [ printTest { input = TmVar 0 1, expected = "[bad index]" }
            ]
        ]


toBeTrue : Bool -> Expectation
toBeTrue value =
    if value then
        Expect.pass

    else
        Expect.fail "value is not true"


type alias ValueTestCase =
    { input : Term
    }


isValueTest : ValueTestCase -> Test
isValueTest testCase =
    test (Expression.toString testCase.input) <|
        \_ ->
            toBeTrue (isValue empty testCase.input)


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
                        |> General.eval (Expression.step empty)
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
