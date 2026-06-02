module GenericPage exposing (Project, create)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Parser exposing (Parser)


type alias Project a =
    { title : String
    , parse : String -> Maybe a
    , printer : a -> String
    , evaluator : a -> a
    }


create : Project a -> Program () (Model a) (Msg a)
create project =
    Browser.element
        { init = init project
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model a
    = State
        { project : Project a
        , input : String
        , cache : List ( String, a, a )
        }


type Msg a
    = InputChanged String
    | AddToCache ( String, a, a )


init : Project a -> () -> ( Model a, Cmd (Msg a) )
init project _ =
    ( State { project = project, input = "", cache = [] }, Cmd.none )


view : Model a -> Html (Msg a)
view ((State m) as model) =
    Html.div []
        [ Html.h1 [] [ Html.text m.project.title ]
        , viewInput model
        , viewTerm model
        , viewCache model
        ]


viewInput : Model a -> Html (Msg a)
viewInput (State { input }) =
    Html.textarea [ Attribute.cols 80, Attribute.rows 10, Event.onInput InputChanged ] [ Html.text input ]


viewTerm : Model a -> Html (Msg a)
viewTerm (State { project, input }) =
    case project.parse input of
        Just term ->
            Html.div []
                [ Html.pre []
                    [ Html.code [] [ Html.text (project.printer term) ]
                    ]
                , Html.samp []
                    [ Html.code [] [ Html.text (term |> project.evaluator |> project.printer) ] ]
                ]

        Nothing ->
            Html.pre [] [ Html.text "Could not parse input" ]


viewCache : Model a -> Html (Msg a)
viewCache ((State { project, cache }) as model) =
    let
        body =
            List.map (viewCacheLine project.printer) cache
    in
    Html.div []
        [ viewAddToCache model
        , Html.table []
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Input" ]
                    , Html.th [] [ Html.text "Term" ]
                    , Html.th [] [ Html.text "Normal Form" ]
                    ]
                ]
            , Html.tbody [] body
            ]
        ]


viewAddToCache : Model a -> Html (Msg a)
viewAddToCache (State { project, input }) =
    let
        attributes =
            case project.parse input of
                Just term ->
                    [ Event.onClick (AddToCache ( input, term, project.evaluator term )) ]

                Nothing ->
                    [ Attribute.disabled True ]
    in
    Html.button attributes [ Html.text "+" ]


viewCacheLine : (a -> String) -> ( String, a, a ) -> Html (Msg a)
viewCacheLine printer ( input, term, normal ) =
    Html.tr []
        [ Html.td [] [ Html.text input ]
        , Html.td [] [ Html.text (printer term) ]
        , Html.td [] [ Html.text (printer normal) ]
        ]


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg (State model) =
    case msg of
        InputChanged input ->
            ( State { model | input = input }, Cmd.none )

        AddToCache element ->
            ( State { model | cache = element :: model.cache }, Cmd.none )


subscriptions : Model a -> Sub (Msg a)
subscriptions _ =
    Sub.none
