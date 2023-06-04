module Ui exposing (..)

import Element exposing (Attribute, Element)
import Element.Input


button : List (Attribute msg) -> msg -> Element msg -> Element msg
button attributes onPress label =
    Element.Input.button
        attributes
        { onPress = Just onPress
        , label = label
        }
