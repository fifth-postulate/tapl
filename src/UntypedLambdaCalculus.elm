module UntypedLambdaCalculus exposing (Binding(..), Context, Term(..), default, empty, isValue, parse, parser, step, toString)

import General exposing (Continue(..))
import Parser exposing (Parser, atleast, character, characterClass, complete, many, word)


type Term
    = TmVar Int Int
    | TmAbs String Term
    | TmApp Term Term


toString : Context -> Term -> String
toString context term =
    format context term


type alias Context =
    List ( String, Binding )


type Binding
    = NameBind


empty : Context
empty =
    []


default : Context
default =
    [ ( "x", NameBind )
    , ( "y", NameBind )
    , ( "z", NameBind )
    , ( "u", NameBind )
    , ( "v", NameBind )
    , ( "w", NameBind )
    , ( "a", NameBind )
    , ( "b", NameBind )
    , ( "c", NameBind )
    ]


freshName : Context -> String -> ( Context, String )
freshName context suggestion =
    let
        taken =
            context
                |> List.map Tuple.first

        go : String -> ( Context, String )
        go candidate =
            if List.member candidate taken then
                go (candidate ++ "'")

            else
                ( ( candidate, NameBind ) :: context, candidate )
    in
    go suggestion


length : Context -> Int
length =
    List.length


index2name : Context -> Int -> Maybe String
index2name context index =
    case context of
        ( h, _ ) :: ctx ->
            if index <= 0 then
                Just h

            else
                index2name ctx (index - 1)

        _ ->
            Nothing


format : Context -> Term -> String
format context term =
    case term of
        TmAbs x t ->
            let
                ( ctx, x_ ) =
                    freshName context x
            in
            [ "(lambda ", x_, ". ", format ctx t, ")" ]
                |> String.join ""

        TmApp t1 t2 ->
            [ "(", format context t1, " ", format context t2, ")" ]
                |> String.join ""

        TmVar x _ ->
            x
                |> index2name context
                |> Maybe.withDefault "[bad index]"


shift : Int -> Term -> Term
shift d term =
    let
        walk : Int -> Term -> Term
        walk c trm =
            case trm of
                TmVar x n ->
                    if x >= c then
                        TmVar (x + d) (n + d)

                    else
                        TmVar x (n + d)

                TmAbs x t ->
                    TmAbs x (walk (c + 1) t)

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)
    in
    walk 0 term


substitute : Int -> Term -> Term -> Term
substitute j s term =
    let
        walk : Int -> Term -> Term
        walk c trm =
            case trm of
                TmVar x n ->
                    if x == j + c then
                        shift c s

                    else
                        TmVar x n

                TmAbs x t ->
                    TmAbs x (walk (c + 1) t)

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)
    in
    walk 0 term


substituteTop : Term -> Term -> Term
substituteTop s t =
    shift -1 (substitute 0 (shift 1 s) t)


isValue : Context -> Term -> Bool
isValue _ term =
    case term of
        TmAbs _ _ ->
            True

        _ ->
            False


step : Context -> Term -> Continue Term
step context term =
    case term of
        TmApp ((TmAbs _ t11) as t1) t2 ->
            if isValue context t2 then
                substituteTop t2 t11
                    |> Progressed

            else
                case step context t2 of
                    Progressed t ->
                        TmApp t1 t
                            |> Progressed

                    Stalled ->
                        Stalled

        TmApp t1 t2 ->
            case step context t1 of
                Progressed t ->
                    TmApp t t2
                        |> Progressed

                Stalled ->
                    Stalled

        _ ->
            Stalled


parse : String -> Maybe Term
parse input =
    case complete parser empty input of
        ( _, _, h ) :: _ ->
            Just h

        [] ->
            Nothing


parser : Parser Context Term
parser =
    Parser.ors abstractionParser
        [ variableParser
        , applicationParser
        , Parser.bracketed '(' ')' (Parser.lazy (\_ -> parser))
        ]


variableParser : Parser Context Term
variableParser =
    identifier
        |> Parser.map (always (TmVar 0 1))


identifier : Parser Context String
identifier =
    characterClass 'a' 'z'
        |> Parser.map String.fromChar


abstractionParser : Parser Context Term
abstractionParser =
    word "lambda"
        |> Parser.ignoreThen (atleast 1 space)
        |> Parser.ignoreThen identifier
        |> Parser.keepThenIgnore dot
        |> Parser.followedBy (Parser.lazy (\_ -> parser))
        |> Parser.map (\( id, term ) -> TmAbs id term)


space : Parser Context Char
space =
    character ' '


dot : Parser Context Char
dot =
    many space
        |> Parser.ignoreThen (character '.')
        |> Parser.keepThenIgnore (many space)


applicationParser : Parser Context Term
applicationParser =
    let
        eventuallyTerm =
            Parser.lazy (\_ -> parser)
    in
    Parser.bracketed '(' ')' eventuallyTerm
        |> Parser.keepThenIgnore (many space)
        |> Parser.followedBy eventuallyTerm
        |> Parser.map (\( l, r ) -> TmApp l r)
