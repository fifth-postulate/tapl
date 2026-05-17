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


create : Project a -> Program () (Model a) Msg
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
        }


type Msg
    = InputChanged String


init : Project a -> () -> ( Model a, Cmd Msg )
init project _ =
    ( State { project = project, input = "" }, Cmd.none )


view : Model a -> Html Msg
view ((State m) as model) =
    Html.div []
        [ Html.h1 [] [ Html.text m.project.title ]
        , viewInput model
        , viewTerm model
        ]


viewInput : Model a -> Html Msg
viewInput (State { input }) =
    Html.textarea [ Event.onInput InputChanged ] [ Html.text input ]


viewTerm : Model a -> Html Msg
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


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg (State model) =
    case msg of
        InputChanged input ->
            ( State { model | input = input }, Cmd.none )


subscriptions : Model a -> Sub Msg
subscriptions _ =
    Sub.none
