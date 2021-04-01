module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events as Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int



-- assign inital model to 0


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


type Update
    = SoftwareUpdate
    | FirmwareUpdate


update : Msg -> Model -> Model
update msg model =
    let
        minus =
            5
    in
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


type alias Person =
    { name : String, age : Int }



-- VIEW


view : Model -> Html Msg
view model =
    let
        minus =
            -- this comment has a quotes, not is not a "string"
            "-"
    in
    div []
        [ button [ onClick Decrement ] [ text minus ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
