module Ui exposing (..)

import Color
import Color.Convert exposing (hexToColor)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr


colors =
    lightMode


colours =
    colors


lightMode =
    { font = fromHex "#333"
    , orange = fromHex "#FAE6CC"
    , blue = fromHex "#C9DBF8"
    , green = fromHex "#CCE6CC"
    , darkGreen = fromHex "5cb07e"
    , bg = fromHex "#FFFFFF"
    , bg2 = fromHex "#E9E9E9"
    , fg = fromHex "#333333"
    , slide =
        { purple = fromHex "#6371d7"
        }
    }


style =
    { rounding = 10
    , padding = paddingXY 15 10
    }


rounded =
    Border.rounded style.rounding


layout : Element msg -> Html msg
layout elements =
    Element.layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow =
                    Nothing
                }
            ]
        }
        [ padding 16, Font.family [ Font.typeface "Montserrat", Font.typeface "system-ui" ] ]
        elements


button : List (Attribute msg) -> msg -> Element msg -> Element msg
button overrides msg el =
    button_
        ([ Background.color <| colors.blue
         , rounded
         , style.padding
         ]
            ++ overrides
        )
        msg
        el


button_ : List (Attribute msg) -> msg -> Element msg -> Element msg
button_ attributes onPress label =
    Input.button
        attributes
        { onPress = Just onPress
        , label = label
        }


multilineInput : String -> String -> (String -> msg) -> String -> Element msg
multilineInput label placeholderLabel onChange field =
    Input.multiline
        [ Attr.attribute "data-gramm_editor" "false" |> htmlAttribute
        , Attr.attribute "data-enable-grammarly" "false" |> htmlAttribute
        , rounded
        ]
        { text = field
        , placeholder = Just (Input.placeholder [] (text placeholderLabel))
        , onChange = onChange
        , label =
            if label == "" then
                Input.labelHidden placeholderLabel

            else
                Input.labelAbove
                    []
                    (paragraph [] [ text label ])
        , spellcheck = True
        }


fromHex : String -> Color
fromHex str =
    case hexToColor str of
        Ok col ->
            let
                x =
                    Color.toRgba col
            in
            Element.rgba x.red x.green x.blue x.alpha

        Err _ ->
            Element.rgb 255 0 0


hilightWhen condition =
    Background.color
        (if condition then
            colors.green

         else
            colors.bg2
        )


pluralise singular multiple count =
    if count == 1 then
        singular

    else
        multiple
