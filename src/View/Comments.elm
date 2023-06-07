module View.Comments exposing (..)

import Element exposing (..)
import Element.Background as Background
import Identicon
import String.Nonempty
import Types exposing (..)
import Ui


adminView : List Comment -> Element FrontendMsg
adminView comments =
    column
        [ spacing 10, width fill ]
        [ row
            [ spacing 8 ]
            [ el [] (text <| "Comments: " ++ String.fromInt (List.length comments))
            , Ui.button [ Background.color (rgb 1 0.7 0.7) ] PressedRemoveAllBans (text "Remove bans")
            ]
        , comments |> List.map commentView |> column [ width fill, spacing 8 ]
        ]


commentView : Comment -> Element FrontendMsg
commentView comment =
    row
        [ width fill
        , paddingEach { left = 10, right = 5, top = 5, bottom = 5 }
        , Background.color Ui.colors.bg2
        , spacing 10
        , Ui.rounded
        ]
        [ el [] <| html <| Identicon.identicon "30px" comment.sessionId
        , paragraph []
            [ comment.text
                |> String.Nonempty.toString
                |> text
            ]
        , Ui.button [ Background.color (rgb 1 0.7 0.7) ] (PressedBanUser comment.sessionId) (text "Ban")
        ]
