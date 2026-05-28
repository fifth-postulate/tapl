module Parser exposing (Parser, atleast, bracketed, character, characterClass, complete, followedBy, ignoreThen, keepThenIgnore, lazy, many, map, or, ors, word)

import List exposing (concat)


type alias Parser a =
    String -> List ( String, a )


character : Char -> Parser Char
character needle =
    word (String.fromChar needle)
        |> map (always needle)


characterClass : Char -> Char -> Parser Char
characterClass minimum maximum =
    let
        mini =
            Char.toCode minimum

        maxi =
            Char.toCode maximum

        withinRange : Char -> Bool
        withinRange c =
            let
                code =
                    Char.toCode c
            in
            mini <= code && code <= maxi
    in
    predicate withinRange


predicate : (Char -> Bool) -> Parser Char
predicate p input =
    case String.uncons input of
        Just ( head, tail ) ->
            if p head then
                ( tail, head )
                    |> List.singleton

            else
                []

        Nothing ->
            []


word : String -> Parser String
word target input =
    if String.startsWith target input then
        ( String.dropLeft (String.length target) input, target )
            |> List.singleton

    else
        []


map : (a -> b) -> Parser a -> Parser b
map f parser input =
    input
        |> parser
        |> List.map (Tuple.mapSecond f)


followedBy : Parser b -> Parser a -> Parser ( a, b )
followedBy second first input =
    let
        proceedWithSecond : ( String, a ) -> List ( String, ( a, b ) )
        proceedWithSecond ( rest, result ) =
            rest
                |> second
                |> List.map (Tuple.mapSecond (Tuple.pair result))
    in
    input
        |> first
        |> List.map proceedWithSecond
        |> concat


ignoreThen : Parser b -> Parser a -> Parser b
ignoreThen second first =
    first
        |> followedBy second
        |> map Tuple.second


keepThenIgnore : Parser b -> Parser a -> Parser a
keepThenIgnore second first =
    first
        |> followedBy second
        |> map Tuple.first


or : Parser a -> Parser a -> Parser a
or left right input =
    List.concat [ left input, right input ]


ors : Parser a -> List (Parser a) -> Parser a
ors head tail =
    List.foldl or head tail


many : Parser a -> Parser (List a)
many parser =
    let
        tryParser =
            parser
                |> followedBy (lazy (\_ -> many parser))
                |> map (\( head, tail ) -> head :: tail)
    in
    or tryParser (succeed [])


atleast : Int -> Parser a -> Parser (List a)
atleast n parser =
    repeat n parser
        |> followedBy (many parser)
        |> map (\( first, second ) -> first ++ second)


repeat : Int -> Parser a -> Parser (List a)
repeat n parser =
    if n <= 0 then
        succeed []

    else
        parser
            |> followedBy (lazy (\_ -> repeat (n - 1) parser))
            |> map (\( head, tail ) -> head :: tail)


lazy : (() -> Parser a) -> Parser a
lazy produce input =
    produce () input


succeed : a -> Parser a
succeed a input =
    [ ( input, a ) ]


fail : Parser a
fail _ =
    []


bracketed : Char -> Char -> Parser a -> Parser a
bracketed left right parser =
    character left
        |> ignoreThen parser
        |> keepThenIgnore (character right)


complete : Parser a -> Parser a
complete parser input =
    input
        |> parser
        |> List.filter (Tuple.first >> String.isEmpty)
