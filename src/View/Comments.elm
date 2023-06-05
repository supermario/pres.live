module View.Comments exposing (..)

import Element exposing (..)
import Element.Background as Background
import Identicon
import String.Nonempty
import Types exposing (..)
import Ui


adminView : List Comment -> Element FrontendMsg
adminView comments =
    column [ spacing 10 ]
        [ el [] (text <| "Comments: " ++ String.fromInt (List.length comments))
        , comments |> List.take 20 |> List.map commentView |> column [ width fill, spacing 8 ]
        ]


commentView : Comment -> Element FrontendMsg
commentView comment =
    row
        [ width fill, paddingXY 10 5, Background.color Ui.colors.bg2, spacing 10, Ui.rounded ]
        [ el [] <| html <| Identicon.identicon "30px" comment.sessionId
        , paragraph []
            [ comment.text
                |> String.Nonempty.toString
                |> text
            ]
        ]
