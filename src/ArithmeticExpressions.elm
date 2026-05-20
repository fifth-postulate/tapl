module ArithmeticExpressions exposing (Term(..), eval, fromInt, isNumerical, isValue, parse, toString)

import Parser exposing (Parser, atleast, bracketed, character, ignoreThen, keepThenIgnore, many, or)


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
    Parser.word "true"
        |> Parser.map (always TmTrue)


falseParser : Parser Term
falseParser =
    Parser.word "false"
        |> Parser.map (always TmFalse)


succParser : Parser Term
succParser =
    Parser.word "succ"
        |> Parser.ignoreThen (appliedTo (Parser.lazy (\_ -> parser)))
        |> Parser.map TmSucc


appliedTo : Parser a -> Parser a
appliedTo argument =
    or
        (bracketed '(' ')' argument)
        (atleast 1 space |> ignoreThen argument)


space : Parser Char
space =
    character ' '


predParser : Parser Term
predParser =
    Parser.word "pred"
        |> Parser.ignoreThen (appliedTo (Parser.lazy (\_ -> parser)))
        |> Parser.map TmPred


isZeroParser : Parser Term
isZeroParser =
    Parser.word "iszero"
        |> Parser.ignoreThen (appliedTo (Parser.lazy (\_ -> parser)))
        |> Parser.map TmIsZero


ifParser : Parser Term
ifParser =
    let
        eventuallyTerm : Parser Term
        eventuallyTerm =
            appliedTo (Parser.lazy (\_ -> parser))
                |> keepThenIgnore (many space)

        eventuallyLastTerm : Parser Term
        eventuallyLastTerm =
            appliedTo (Parser.lazy (\_ -> parser))
    in
    Parser.word "if"
        |> Parser.ignoreThen eventuallyTerm
        |> Parser.keepThenIgnore (Parser.word "then")
        |> Parser.followedBy eventuallyTerm
        |> Parser.keepThenIgnore (Parser.word "else")
        |> Parser.followedBy eventuallyLastTerm
        |> Parser.map (\( ( guard, ifTrue ), ifFalse ) -> TmIf guard ifTrue ifFalse)


toString : Term -> String
toString term =
    case term of
        TmTrue ->
            "true"

        TmFalse ->
            "false"

        TmZero ->
            "O"

        TmSucc t ->
            "succ(" ++ toString t ++ ")"

        TmPred t ->
            "pred(" ++ toString t ++ ")"

        TmIsZero t ->
            "iszero(" ++ toString t ++ ")"

        TmIf guard ifTrue ifFalse ->
            "if(" ++ toString guard ++ ")then(" ++ toString ifTrue ++ ")else(" ++ toString ifFalse ++ ")"
