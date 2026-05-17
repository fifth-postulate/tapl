module ArithmeticExpressions exposing (Term(..), eval, fromInt, isNumerical, isValue, parse, toString)

import Parser exposing (Parser)


type Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term


isNumerical : Term -> Bool
isNumerical term =
    case term of
        TmZero ->
            True

        TmSucc t ->
            isNumerical t

        _ ->
            False


fromInt : Int -> Term
fromInt n =
    List.foldl (\f acc -> f acc) TmZero (List.repeat n TmSucc)


isValue : Term -> Bool
isValue term =
    case term of
        TmFalse ->
            True

        TmTrue ->
            True

        _ ->
            isNumerical term


type Continue
    = Progressed Term
    | Stalled


step : Term -> Continue
step term =
    case term of
        TmIf TmTrue t _ ->
            Progressed t

        TmIf TmFalse _ t ->
            Progressed t

        TmIf guard l r ->
            case step guard of
                Progressed t ->
                    Progressed (TmIf t l r)

                Stalled ->
                    Stalled

        TmSucc s ->
            case step s of
                Progressed t ->
                    Progressed (TmSucc t)

                Stalled ->
                    Stalled

        TmPred TmZero ->
            Progressed TmZero

        TmPred (TmSucc v) ->
            if isNumerical v then
                Progressed v

            else
                Stalled

        TmPred s ->
            case step s of
                Progressed t ->
                    Progressed (TmPred t)

                Stalled ->
                    Stalled

        TmIsZero TmZero ->
            Progressed TmTrue

        TmIsZero (TmSucc v) ->
            if isNumerical v then
                Progressed TmFalse

            else
                Stalled

        TmIsZero s ->
            case step s of
                Progressed t ->
                    Progressed (TmIsZero t)

                Stalled ->
                    Stalled

        _ ->
            Stalled


eval : Term -> Term
eval term =
    case step term of
        Progressed t ->
            eval t

        Stalled ->
            term


parse : String -> Maybe Term
parse input =
    case parser input of
        ( _, h ) :: _ ->
            Just h

        [] ->
            Nothing


parser : Parser Term
parser =
    Parser.ors zeroParser
        [ trueParser
        , falseParser
        , succParser
        , predParser
        , isZeroParser
        , ifParser
        , Parser.bracketed '(' ')' (Parser.lazy (\_ -> parser))
        ]


zeroParser : Parser Term
zeroParser =
    Parser.character 'O'
        |> Parser.map (always TmZero)


trueParser : Parser Term
trueParser =
    Parser.character 'T'
        |> Parser.map (always TmTrue)


falseParser : Parser Term
falseParser =
    Parser.character 'F'
        |> Parser.map (always TmFalse)


succParser : Parser Term
succParser =
    Parser.character 'S'
        |> Parser.ignoreThen (Parser.lazy (\_ -> parser))
        |> Parser.map TmSucc


predParser : Parser Term
predParser =
    Parser.character 'P'
        |> Parser.ignoreThen (Parser.lazy (\_ -> parser))
        |> Parser.map TmPred


isZeroParser : Parser Term
isZeroParser =
    Parser.character '?'
        |> Parser.ignoreThen (Parser.lazy (\_ -> parser))
        |> Parser.map TmIsZero


ifParser : Parser Term
ifParser =
    let
        eventuallyTerm : Parser Term
        eventuallyTerm =
            Parser.lazy (\_ -> parser)
    in
    Parser.word "if"
        |> Parser.ignoreThen eventuallyTerm
        |> Parser.followedBy eventuallyTerm
        |> Parser.followedBy eventuallyTerm
        |> Parser.map (\( ( guard, ifTrue ), ifFalse ) -> TmIf guard ifTrue ifFalse)


toString : Term -> String
toString term =
    case term of
        TmTrue ->
            "T"

        TmFalse ->
            "F"

        TmZero ->
            "O"

        TmSucc t ->
            "S(" ++ toString t ++ ")"

        TmPred t ->
            "P(" ++ toString t ++ ")"

        TmIsZero t ->
            "?(" ++ toString t ++ ")"

        TmIf guard ifTrue ifFalse ->
            "if(" ++ toString guard ++ ")(" ++ toString ifTrue ++ ")(" ++ toString ifFalse ++ ")"
