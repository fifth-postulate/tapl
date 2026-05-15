module ArithmeticExpressions exposing (Term(..), eval, fromInt, isNumerical, isValue)


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
    let
        go : Term -> Int -> Term
        go acc current =
            if current <= 0 then
                acc

            else
                go (TmSucc acc) (n - 1)
    in
    go TmZero n


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
