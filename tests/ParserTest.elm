module ParserTest exposing (suite)

import Dict exposing (Dict)
import Expect
import Parser exposing (Parser, atleast, character, characterClass, complete, followedBy, ignoreThen, keepThenIgnore, many, or, word)
import Test exposing (..)


suite : Test
suite =
    describe "Parser"
        [ verify
            { description = "character test 1"
            , parser = character 'a'
            , on = "a"
            , context = ()
            , expected = [ ( (), "", 'a' ) ]
            }
        , verify
            { description = "character test 2"
            , parser = character 'a'
            , on = "b"
            , context = ()
            , expected = []
            }
        , verify
            { description = "word test 1"
            , parser = word "Hello"
            , on = "Hello, World!"
            , context = ()
            , expected = [ ( (), ", World!", "Hello" ) ]
            }
        , verify
            { description = "followedBy test 1"
            , parser = character 'a' |> followedBy (character 'b')
            , on = "abc"
            , context = ()
            , expected = [ ( (), "c", ( 'a', 'b' ) ) ]
            }
        , verify
            { description = "ignoreThen test 1"
            , parser = character 'a' |> ignoreThen (character 'b')
            , on = "abc"
            , context = ()
            , expected = [ ( (), "c", 'b' ) ]
            }
        , verify
            { description = "keepThenIgnore test 1"
            , parser = character 'a' |> keepThenIgnore (character 'b')
            , on = "abc"
            , context = ()
            , expected = [ ( (), "c", 'a' ) ]
            }
        , verify
            { description = "or test 1"
            , parser = or (character 'a') (character 'b')
            , on = "ac"
            , context = ()
            , expected = [ ( (), "c", 'a' ) ]
            }
        , verify
            { description = "or test 2"
            , parser = or (character 'a') (character 'b')
            , on = "bc"
            , context = ()
            , expected = [ ( (), "c", 'b' ) ]
            }
        , verify
            { description = "many test 1"
            , parser = many (character 'a')
            , on = "b"
            , context = ()
            , expected = [ ( (), "b", [] ) ]
            }
        , verify
            { description = "many test 2"
            , parser = many (character 'a')
            , on = "ab"
            , context = ()
            , expected = [ ( (), "b", [ 'a' ] ), ( (), "ab", [] ) ]
            }
        , verify
            { description = "many test 3"
            , parser = many (character 'a')
            , on = "aab"
            , context = ()
            , expected = [ ( (), "b", [ 'a', 'a' ] ), ( (), "ab", [ 'a' ] ), ( (), "aab", [] ) ]
            }
        , verify
            { description = "atleast 1 test 1"
            , parser = atleast 2 (character 'a')
            , on = "aaaab"
            , context = ()
            , expected = [ ( (), "b", [ 'a', 'a', 'a', 'a' ] ), ( (), "ab", [ 'a', 'a', 'a' ] ), ( (), "aab", [ 'a', 'a' ] ) ]
            }
        , verify
            { description = "character class test 1"
            , parser = characterClass 'a' 'c'
            , on = "a"
            , context = ()
            , expected = [ ( (), "", 'a' ) ]
            }
        , verify
            { description = "character class test 2"
            , parser = characterClass 'a' 'c'
            , on = "b"
            , context = ()
            , expected = [ ( (), "", 'b' ) ]
            }
        , verify
            { description = "character class test 3"
            , parser = characterClass 'a' 'c'
            , on = "c"
            , context = ()
            , expected = [ ( (), "", 'c' ) ]
            }
        , verify
            { description = "character class test 4"
            , parser = characterClass 'a' 'c'
            , on = "d"
            , context = ()
            , expected = []
            }
        , verify
            { description = "complete test 1"
            , parser = complete (many (character 'a'))
            , on = "aaa"
            , context = ()
            , expected = [ ( (), "", [ 'a', 'a', 'a' ] ) ]
            }
        , verify
            { description = "update context test 1"
            , parser = many (character 'a' |> Parser.updateContext (\n -> n + 1))
            , on = "aaa"
            , context = 0
            , expected = [ ( 3, "", [ 'a', 'a', 'a' ] ), ( 2, "a", [ 'a', 'a' ] ), ( 1, "aa", [ 'a' ] ), ( 0, "aaa", [] ) ]
            }
        , verify
            { description = "update context test 2"
            , parser = many (character 'b' |> Parser.updateContext (\n -> n + 1))
            , on = "b"
            , context = 0
            , expected = [ ( 1, "", [ 'b' ] ), ( 0, "b", [] ) ]
            }
        , verify
            { description = "modify context test 1"
            , parser = complete (many (tally (Parser.or (character 'a') (character 'b'))))
            , on = "abaab"
            , context = Dict.empty
            , expected = [ ( Dict.fromList [ ( 'a', 3 ), ( 'b', 2 ) ], "", [ 'a', 'b', 'a', 'a', 'b' ] ) ]
            }
        ]


tally : Parser (Dict Char Int) Char -> Parser (Dict Char Int) Char
tally parser =
    let
        inc : Maybe Int -> Maybe Int
        inc v =
            v
                |> Maybe.map (\n -> n + 1)
                |> Maybe.withDefault 1
                |> Just
    in
    parser
        |> Parser.modifyContext (\a c -> Dict.update a inc c)


type alias ParserTestCase c a =
    { description : String
    , parser : Parser c a
    , on : String
    , context : c
    , expected : List ( c, String, a )
    }


verify : ParserTestCase c a -> Test
verify testCase =
    test testCase.description <|
        \_ ->
            let
                actual =
                    testCase.parser testCase.context testCase.on
            in
            Expect.equal actual testCase.expected
