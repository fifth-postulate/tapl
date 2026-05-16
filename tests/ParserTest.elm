module ParserTest exposing (suite)

import Expect exposing (Expectation)
import Parser exposing (Parser, character, followedBy, ignoreThen, keepThenIgnore, many, or, word)
import Test exposing (..)


suite : Test
suite =
    describe "Parser"
        [ verify
            { description = "character test 1"
            , parser = character 'a'
            , on = "a"
            , expected = [ ( "", 'a' ) ]
            }
        , verify
            { description = "character test 2"
            , parser = character 'a'
            , on = "b"
            , expected = []
            }
        , verify
            { description = "word test 1"
            , parser = word "Hello"
            , on = "Hello, World!"
            , expected = [ ( ", World!", "Hello" ) ]
            }
        , verify
            { description = "followedBy test 1"
            , parser = character 'a' |> followedBy (character 'b')
            , on = "abc"
            , expected = [ ( "c", ( 'a', 'b' ) ) ]
            }
        , verify
            { description = "ignoreThen test 1"
            , parser = character 'a' |> ignoreThen (character 'b')
            , on = "abc"
            , expected = [ ( "c", 'b' ) ]
            }
        , verify
            { description = "keepThenIgnore test 1"
            , parser = character 'a' |> keepThenIgnore (character 'b')
            , on = "abc"
            , expected = [ ( "c", 'a' ) ]
            }
        , verify
            { description = "or test 1"
            , parser = or (character 'a') (character 'b')
            , on = "ac"
            , expected = [ ( "c", 'a' ) ]
            }
        , verify
            { description = "or test 2"
            , parser = or (character 'a') (character 'b')
            , on = "bc"
            , expected = [ ( "c", 'b' ) ]
            }
        , verify
            { description = "many test 1"
            , parser = many (character 'a')
            , on = "b"
            , expected = [ ( "b", [] ) ]
            }
        , verify
            { description = "many test 2"
            , parser = many (character 'a')
            , on = "ab"
            , expected = [ ( "b", [ 'a' ] ), ( "ab", [] ) ]
            }
        , verify
            { description = "many test 3"
            , parser = many (character 'a')
            , on = "aab"
            , expected = [ ( "b", [ 'a', 'a' ] ), ( "ab", [ 'a' ] ), ( "aab", [] ) ]
            }
        ]


type alias ParserTestCase a =
    { description : String
    , parser : Parser a
    , on : String
    , expected : List ( String, a )
    }


verify : ParserTestCase a -> Test
verify testCase =
    test testCase.description <|
        \_ ->
            let
                actual =
                    testCase.parser testCase.on
            in
            Expect.equal actual testCase.expected
