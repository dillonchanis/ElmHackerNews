module Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "notification is-danger elevation" ]
                [ button [ class "delete", onClick msg ] [ text "" ]
                , text message
                ]

        Nothing ->
            text ""
