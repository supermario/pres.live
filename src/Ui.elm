module Ui exposing (..)

import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Input


button_ : msg -> Element msg -> Element msg
button_ msg el =
    button
        [ Element.padding 8
        , Element.Background.color <| Element.rgb 0.9 0.9 0.9
        , Element.Border.width 1
        , Element.Border.color <| Element.rgb 0.1 0.1 0.1
        ]
        msg
        el


button : List (Attribute msg) -> msg -> Element msg -> Element msg
button attributes onPress label =
    Element.Input.button
        attributes
        { onPress = Just onPress
        , label = label
        }
