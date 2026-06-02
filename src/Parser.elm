module Parser exposing (Parser, alter, atleast, bracketed, character, characterClass, complete, followedBy, ignoreThen, keepThenIgnore, lazy, many, map, modifyContext, or, ors, scoped, updateContext, word)

import List exposing (concat)


type alias Parser c a =
    c -> String -> List ( c, String, a )


character : Char -> Parser c Char
character needle =
    word (String.fromChar needle)
        |> map (always needle)


characterClass : Char -> Char -> Parser c Char
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


predicate : (Char -> Bool) -> Parser c Char
predicate p context input =
    case String.uncons input of
        Just ( head, tail ) ->
            if p head then
                ( context, tail, head )
                    |> List.singleton

            else
                []

        Nothing ->
            []


word : String -> Parser c String
word target context input =
    if String.startsWith target input then
        ( context, String.dropLeft (String.length target) input, target )
            |> List.singleton

    else
        []


map : (a -> b) -> Parser c a -> Parser c b
map f parser context input =
    input
        |> parser context
        |> List.map (\( c, s, a ) -> ( c, s, f a ))


followedBy : Parser c b -> Parser c a -> Parser c ( a, b )
followedBy second first context input =
    let
        proceedWithSecond : ( c, String, a ) -> List ( c, String, ( a, b ) )
        proceedWithSecond ( ctx, rest, result ) =
            rest
                |> second ctx
                |> List.map (\( c, s, b ) -> ( c, s, ( result, b ) ))
    in
    input
        |> first context
        |> List.map proceedWithSecond
        |> concat


ignoreThen : Parser c b -> Parser c a -> Parser c b
ignoreThen second first =
    first
        |> followedBy second
        |> map Tuple.second


keepThenIgnore : Parser c b -> Parser c a -> Parser c a
keepThenIgnore second first =
    first
        |> followedBy second
        |> map Tuple.first


or : Parser c a -> Parser c a -> Parser c a
or left right context input =
    List.concat [ left context input, right context input ]


ors : Parser c a -> List (Parser c a) -> Parser c a
ors head tail =
    List.foldl or head tail


many : Parser c a -> Parser c (List a)
many parser =
    let
        tryParser =
            parser
                |> followedBy (lazy (\_ -> many parser))
                |> map (\( head, tail ) -> head :: tail)
    in
    or tryParser (succeed [])


atleast : Int -> Parser c a -> Parser c (List a)
atleast n parser =
    repeat n parser
        |> followedBy (many parser)
        |> map (\( first, second ) -> first ++ second)


repeat : Int -> Parser c a -> Parser c (List a)
repeat n parser =
    if n <= 0 then
        succeed []

    else
        parser
            |> followedBy (lazy (\_ -> repeat (n - 1) parser))
            |> map (\( head, tail ) -> head :: tail)


lazy : (() -> Parser c a) -> Parser c a
lazy produce context input =
    produce () context input


succeed : a -> Parser c a
succeed a context input =
    [ ( context, input, a ) ]


fail : Parser c a
fail _ _ =
    []


bracketed : Char -> Char -> Parser c a -> Parser c a
bracketed left right parser =
    character left
        |> ignoreThen parser
        |> keepThenIgnore (character right)


complete : Parser c a -> Parser c a
complete parser context input =
    input
        |> parser context
        |> List.filter (\( _, s, _ ) -> String.isEmpty s)


updateContext : (c -> c) -> Parser c a -> Parser c a
updateContext f parser context input =
    input
        |> parser context
        |> List.map (\( c, s, a ) -> ( f c, s, a ))


modifyContext : (a -> c -> c) -> Parser c a -> Parser c a
modifyContext f parser context input =
    input
        |> parser context
        |> List.map (\( c, s, a ) -> ( f a c, s, a ))


alter : (c -> a -> b) -> Parser c a -> Parser c b
alter f parser context input =
    input
        |> parser context
        |> List.map
            (\( c, s, a ) ->
                let
                    b =
                        f c a
                in
                ( c, s, b )
            )


scoped : Parser c a -> Parser c a
scoped parser context input =
    updateContext (always context) parser context input
