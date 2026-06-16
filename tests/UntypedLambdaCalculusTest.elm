module UntypedLambdaCalculusTest exposing (suite)

import Expect exposing (Expectation)
import General
import Language.UntypedLambdaCalculus as Expression exposing (Binding(..), Context, Term(..), default, empty, isValue, parse)
import Test exposing (..)


suite : Test
suite =
    describe "Untyped Lambda Calculus"
        [ describe "isValue"
            [ isValueTest { input = TmAbs "x" (TmVar 0 1) }
            , isValueTest { input = TmAbs "y" (TmVar 0 1) }
            ]
        , describe "eval"
            [ evalTest { input = TmVar 0 1, expected = TmVar 0 1 }
            , evalTest { input = TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "y" (TmVar 0 1)), expected = TmAbs "y" (TmVar 0 1) }
            ]
        , describe "parse"
            [ parseTest { input = "x", context = [ ( "x", NameBind ) ], expected = TmVar 0 1 }
            , parseTest { input = "y", context = [ ( "x", NameBind ), ( "y", NameBind ) ], expected = TmVar 1 2 }
            , parseTest { input = "lambda x. x", context = empty, expected = TmAbs "x" (TmVar 0 1) }
            , parseTest { input = "lambda x . x", context = empty, expected = TmAbs "x" (TmVar 0 1) }
            , parseTest { input = "lambda y. y", context = empty, expected = TmAbs "y" (TmVar 0 1) }
            , parseTest { input = "(lambda y. y)", context = empty, expected = TmAbs "y" (TmVar 0 1) }
            , parseTest { input = "(lambda y. y) (lambda x. x)", context = empty, expected = TmApp (TmAbs "y" (TmVar 0 1)) (TmAbs "x" (TmVar 0 1)) }
            , parseTest { input = "(lambda y. y) (lambda y. y)", context = empty, expected = TmApp (TmAbs "y" (TmVar 0 1)) (TmAbs "y" (TmVar 0 1)) }
            , parseTest { input = "(lambda x. (x) y) (lambda y. y)", context = [ ( "y", NameBind ) ], expected = TmApp (TmAbs "x" (TmApp (TmVar 0 2) (TmVar 1 2))) (TmAbs "y" (TmVar 0 2)) }
            ]
        , describe "print"
            [ printTest { input = TmVar 0 1, context = [ ( "x", NameBind ) ], expected = "x" }
            , printTest { input = TmVar 0 1, context = [ ( "z", NameBind ) ], expected = "z" }
            , printTest { input = TmAbs "x" (TmVar 0 2), context = [ ( "x", NameBind ) ], expected = "(lambda x'. x')" }
            , printTest { input = TmAbs "x" (TmVar 0 1), context = empty, expected = "(lambda x. x)" }
            , printTest { input = TmAbs "x" (TmVar 1 2), context = [ ( "u", NameBind ) ], expected = "(lambda x. u)" }
            , printTest { input = TmAbs "x" (TmVar 1 2), context = [ ( "v", NameBind ) ], expected = "(lambda x. v)" }
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
    test (Expression.toString default testCase.input) <|
        \_ ->
            toBeTrue (isValue empty testCase.input)


type alias EvalTestCase =
    { input : Term
    , expected : Term
    }


evalTest : EvalTestCase -> Test
evalTest testCase =
    test (Expression.toString default testCase.input) <|
        \_ ->
            let
                actual =
                    testCase.input
                        |> General.eval (Expression.step empty)
            in
            Expect.equal actual testCase.expected


type alias ParseTestCase =
    { input : String
    , context : Context
    , expected : Term
    }


parseTest : ParseTestCase -> Test
parseTest testCase =
    test testCase.input <|
        \_ ->
            let
                actual =
                    parse testCase.context testCase.input
            in
            Expect.equal actual (Just testCase.expected)


type alias PrintTestCase =
    { input : Term
    , context : Context
    , expected : String
    }


printTest : PrintTestCase -> Test
printTest testCase =
    test testCase.expected <|
        \_ ->
            let
                actual =
                    Expression.toString testCase.context testCase.input
            in
            Expect.equal actual testCase.expected
