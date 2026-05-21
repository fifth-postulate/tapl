module General exposing (Continue(..), eval)


type Continue a
    = Progressed a
    | Stalled


eval : (a -> Continue a) -> a -> a
eval step original =
    let
        go : a -> a
        go term =
            case step term of
                Progressed t ->
                    go t

                Stalled ->
                    term
    in
    go original
