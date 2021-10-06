module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Html
import Html.Attributes as Attr
import Lamdera
import List.Extra as List
import Types exposing (..)
import Url
import Url.Parser
import Url.Parser.Query


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


decodeUrl : Url.Parser.Parser (Maybe String -> a) a
decodeUrl =
    Url.Parser.query (Url.Parser.Query.string "secret")


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url _ =
    ( case Url.Parser.parse decodeUrl url of
        Just (Just secret) ->
            if secret == Env.secret then
                IsAdmin
                    HowAreYou_
                    { howAreYou = []
                    , howExperiencedAreYouWithElm = []
                    , howExperiencedAreYouWithProgramming = []
                    , whatCountryAreYouFrom = []
                    }

            else
                IsUser (HowAreYou Nothing)

        _ ->
            IsUser (HowAreYou Nothing)
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
                    ( model, Nav.load url )

        UrlChanged url ->
            ( model, Cmd.none )

        PressedHowAreYou happiness ->
            case model of
                IsAdmin _ _ ->
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
                IsAdmin _ _ ->
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
                IsAdmin _ _ ->
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
                IsAdmin _ _ ->
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
        UpdateAdmin answerData ->
            case model of
                IsAdmin currentQuestion _ ->
                    ( IsAdmin currentQuestion answerData, Cmd.none )

                IsUser _ ->
                    ( model, Cmd.none )

        NextQuestion ->
            case model of
                IsAdmin currentQuestion adminData ->
                    ( IsAdmin (nextCurrentQuestion currentQuestion) adminData, Cmd.none )

                IsUser question ->
                    ( nextQuestion question |> IsUser, Cmd.none )


nextCurrentQuestion : CurrentQuestion -> CurrentQuestion
nextCurrentQuestion currentQuestion =
    case currentQuestion of
        HowAreYou_ ->
            HowExperiencedAreYouWithElm_

        HowExperiencedAreYouWithElm_ ->
            HowExperiencedAreYouWithProgramming_

        HowExperiencedAreYouWithProgramming_ ->
            WhatCountryAreYouFrom_

        WhatCountryAreYouFrom_ ->
            WhatCountryAreYouFrom_


nextQuestion : Question -> Question
nextQuestion currentQuestion =
    case currentQuestion of
        HowAreYou _ ->
            HowExperiencedAreYouWithElm Nothing

        HowExperiencedAreYouWithElm _ ->
            HowExperiencedAreYouWithProgramming Nothing

        HowExperiencedAreYouWithProgramming _ ->
            WhatCountryAreYouFrom Nothing

        WhatCountryAreYouFrom _ ->
            WhatCountryAreYouFrom Nothing


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Elm Online Survey!"
    , body =
        [ Element.layout
            []
            (case model of
                IsAdmin currentQuestion answerData ->
                    adminQuestionView currentQuestion answerData

                IsUser question ->
                    questionView question
            )
        ]
    }


adminQuestionView : CurrentQuestion -> AdminData -> Element FrontendMsg
adminQuestionView currentQuestion adminData =
    case currentQuestion of
        HowAreYou_ ->
            adminHowAreYou adminData.howAreYou

        HowExperiencedAreYouWithElm_ ->
            Debug.todo ""

        HowExperiencedAreYouWithProgramming_ ->
            Debug.todo ""

        WhatCountryAreYouFrom_ ->
            Debug.todo ""


adminHowAreYou : List Happiness -> Element msg
adminHowAreYou happinesses =
    Element.column
        [ Element.spacing 16, Element.centerX, Element.centerY ]
        [ happinessQuestionTitle
        , List.map
            (\answer ->
                let
                    count =
                        List.count ((==) answer) happinesses
                in
                happinessToString answer
                    ++ " "
                    ++ String.fromInt count
                    |> Element.text
            )
            happinessAnswers
            |> Element.row [ Element.spacing 8 ]
        ]


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
        [ happinessQuestionTitle
        , answers PressedHowAreYou happinessToString happinessAnswers maybeHappiness
        ]


happinessQuestionTitle =
    Element.paragraph [ Element.Font.center ] [ Element.text "How are you doing?" ]


happinessAnswers =
    [ Good, NotGood ]


happinessToString : Happiness -> String
happinessToString howAreYou =
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
