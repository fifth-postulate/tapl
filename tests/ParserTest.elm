module ParserTest exposing (suite)

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
        ]


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
