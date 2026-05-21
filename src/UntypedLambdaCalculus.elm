module UntypedLambdaCalculus exposing (Term(..), empty, parse, step, toString)

import General exposing (Continue(..))


type Term
    = TmVar Int Int
    | TmAbs String Term
    | TmApp Term Term


toString : Term -> String
toString term =
    format empty term


type alias Context =
    List ( String, Binding )


type Binding
    = NameBind


empty : Context
empty =
    []


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


index2name : Context -> Int -> String
index2name context index =
    case context of
        ( h, _ ) :: ctx ->
            if index <= 0 then
                h

            else
                index2name ctx (index - 1)

        _ ->
            "?"


format : Context -> Term -> String
format context term =
    case term of
        TmAbs x t ->
            let
                ( ctx, x_ ) =
                    freshName context x
            in
            [ "(lambda ", x_, ", ", format ctx t, ")" ]
                |> String.join ""

        TmApp t1 t2 ->
            [ "(", format context t1, " ", format context t2, ")" ]
                |> String.join ""

        TmVar x n ->
            if length context == n then
                index2name context x

            else
                "[bad index]"


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
parse _ =
    Nothing
