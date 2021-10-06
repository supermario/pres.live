module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes as Attr
import Lamdera
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url _ =
    ( IsUser (HowAreYou Nothing)
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        PressedHowAreYou happiness ->
            case model of
                IsAdmin ->
                    ( model, Cmd.none )

                IsUser question ->
                    case question of
                        HowAreYou _ ->
                            ( Just happiness |> HowAreYou |> IsUser
                            , Lamdera.sendToBackend (ChoseHowAreYou happiness)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithElm experienceLevel ->
            case model of
                IsAdmin ->
                    ( model, Cmd.none )

                IsUser question ->
                    case question of
                        HowExperiencedAreYouWithElm _ ->
                            ( Just experienceLevel |> HowExperiencedAreYouWithElm |> IsUser
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithElm experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithProgramming experienceLevel ->
            case model of
                IsAdmin ->
                    ( model, Cmd.none )

                IsUser question ->
                    case question of
                        HowExperiencedAreYouWithProgramming _ ->
                            ( Just experienceLevel |> HowExperiencedAreYouWithProgramming |> IsUser
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithProgramming experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedWhatCountryAreYouFrom country ->
            case model of
                IsAdmin ->
                    ( model, Cmd.none )

                IsUser question ->
                    case question of
                        WhatCountryAreYouFrom _ ->
                            ( Just country |> WhatCountryAreYouFrom |> IsUser
                            , Lamdera.sendToBackend (ChoseWhatCountryAreYouFrom country)
                            )

                        _ ->
                            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateAdmin record ->
            Debug.todo ""

        NextQuestion ->
            Debug.todo ""


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            []
            (case model of
                IsAdmin ->
                    Element.text "You are admin"

                IsUser question ->
                    questionView question
            )
        ]
    }


questionView : Question -> Element FrontendMsg
questionView question =
    case question of
        HowAreYou maybeHappiness ->
            happinessQuestionView maybeHappiness

        HowExperiencedAreYouWithElm maybeExperienceLevel ->
            Debug.todo ""

        HowExperiencedAreYouWithProgramming maybeExperienceLevel ->
            Debug.todo ""

        WhatCountryAreYouFrom maybeString ->
            Debug.todo ""


happinessQuestionView maybeHappiness =
    Element.column
        [ Element.spacing 16, Element.centerX, Element.centerY ]
        [ Element.paragraph [ Element.Font.center ] [ Element.text "How are you doing?" ]
        , answers PressedHowAreYou howAreYouToString [ Good, NotGood ] maybeHappiness
        ]


howAreYouToString : Happiness -> String
howAreYouToString howAreYou =
    case howAreYou of
        Good ->
            "Good"

        NotGood ->
            "Not good"


answers : (a -> msg) -> (a -> String) -> List a -> Maybe a -> Element msg
answers onPress toString options selected =
    Element.row [ Element.spacing 8, Element.centerX ]
        (List.map
            (\option ->
                Element.Input.button
                    [ Element.Background.color
                        (if selected == Just option then
                            Element.rgb 0.7 0.8 0.9

                         else
                            Element.rgb 0.9 0.9 0.9
                        )
                    , Element.Border.width 1
                    , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                    , Element.padding 16
                    ]
                    { onPress = Just (onPress option)
                    , label = toString option |> Element.text
                    }
            )
            options
        )
