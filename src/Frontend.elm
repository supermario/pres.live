module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Countries exposing (Country)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
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


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        setNewUrl =
            Browser.Navigation.replaceUrl key (Url.toString { url | query = Nothing })
    in
    case Url.Parser.parse decodeUrl url of
        Just (Just secret) ->
            ( IsUser (HowAreYou Nothing)
            , Cmd.batch
                [ Lamdera.sendToBackend (AdminAuth secret)
                , setNewUrl
                ]
            )

        _ ->
            ( IsUser (HowAreYou Nothing)
            , Cmd.batch
                [ setNewUrl
                ]
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model, Browser.Navigation.load url )

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

        AdminPressedNextQuestion ->
            case model of
                IsAdmin _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestNextQuestion )

                IsUser _ ->
                    ( model, Cmd.none )

        AdminPressedReset ->
            case model of
                IsAdmin _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestReset )

                IsUser _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SetAdminMode currentQuestion answerData ->
            ( IsAdmin currentQuestion answerData, Cmd.none )

        UpdateAdmin answerData ->
            case model of
                IsAdmin currentQuestion _ ->
                    ( IsAdmin currentQuestion answerData, Cmd.none )

                IsUser _ ->
                    ( model, Cmd.none )

        SetCurrentQuestion question ->
            case model of
                IsAdmin _ adminData ->
                    ( IsAdmin question adminData, Cmd.none )

                IsUser _ ->
                    ( currentQuestionToQuestion question |> IsUser, Cmd.none )

        PostCommentResponse ->
            ( model, Cmd.none )


currentQuestionToQuestion : CurrentQuestion -> Question
currentQuestionToQuestion currentQuestion =
    case currentQuestion of
        HowAreYou_ ->
            HowAreYou Nothing

        HowExperiencedAreYouWithElm_ ->
            HowExperiencedAreYouWithElm Nothing

        HowExperiencedAreYouWithProgramming_ ->
            HowExperiencedAreYouWithProgramming Nothing

        WhatCountryAreYouFrom_ ->
            WhatCountryAreYouFrom Nothing


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Elm Online Survey!"
    , body =
        [ Element.layout
            [ Element.padding 16 ]
            (case model of
                IsAdmin currentQuestion answerData ->
                    Element.column
                        [ Element.width Element.fill, Element.height Element.fill, Element.spacing 8 ]
                        [ adminQuestionView currentQuestion answerData
                        , Element.Input.button
                            [ Element.padding 8
                            , Element.Background.color <| Element.rgb 0.9 0.9 0.9
                            , Element.Border.width 1
                            , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                            ]
                            { onPress = Just AdminPressedNextQuestion
                            , label = Element.text "Next Question"
                            }
                        , Element.Input.button
                            [ Element.padding 8
                            , Element.Background.color <| Element.rgb 0.9 0.9 0.9
                            , Element.Border.width 1
                            , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                            ]
                            { onPress = Just AdminPressedReset
                            , label = Element.text "Reset Questions"
                            }
                        ]

                IsUser question ->
                    questionView question
            )
        ]
    }


adminQuestionView : CurrentQuestion -> AdminData -> Element FrontendMsg
adminQuestionView currentQuestion adminData =
    case currentQuestion of
        HowAreYou_ ->
            questionContainer
                happinessQuestionTitle
                (adminAnswers happinessToString happinessAnswers adminData.howAreYou)

        HowExperiencedAreYouWithElm_ ->
            questionContainer
                howExperiencedAreYouWithElmTitle
                (adminAnswers experienceLevelToString experienceLevelAnswers adminData.howExperiencedAreYouWithElm)

        HowExperiencedAreYouWithProgramming_ ->
            questionContainer
                howExperiencedAreYouWithProgrammingTitle
                (adminAnswers experienceLevelToString experienceLevelAnswers adminData.howExperiencedAreYouWithProgramming)

        WhatCountryAreYouFrom_ ->
            questionContainer
                countryQuestionTitle
                (adminAnswers countryToString countryAnswers adminData.whatCountryAreYouFrom)


howExperiencedAreYouWithElmTitle : Element msg
howExperiencedAreYouWithElmTitle =
    Element.paragraph [ Element.Font.center ] [ Element.text "How good are you with Elm?" ]


howExperiencedAreYouWithProgrammingTitle : Element msg
howExperiencedAreYouWithProgrammingTitle =
    Element.paragraph [ Element.Font.center ] [ Element.text "How good are you at programming in general?" ]


countryQuestionTitle : Element msg
countryQuestionTitle =
    Element.paragraph [ Element.Font.center ] [ Element.text "What country do you live in?" ]


adminAnswers : (a -> String) -> List a -> List a -> Element msg
adminAnswers toString possibleAnswers answers_ =
    List.filterMap
        (\answer ->
            let
                count =
                    List.count ((==) answer) answers_
            in
            if count == 0 then
                Nothing

            else
                toString answer
                    ++ " "
                    ++ String.fromInt count
                    |> Element.text
                    |> Element.el
                        [ Element.Background.color <| Element.rgb 0.9 0.9 0.9
                        , Element.Border.width 1
                        , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                        , Element.padding 16
                        ]
                    |> Just
        )
        possibleAnswers
        |> Element.wrappedRow [ Element.spacing 8, Element.centerX ]


questionView : Question -> Element FrontendMsg
questionView question =
    case question of
        HowAreYou maybeHappiness ->
            questionContainer
                happinessQuestionTitle
                (answers PressedHowAreYou happinessToString happinessAnswers maybeHappiness)

        HowExperiencedAreYouWithElm maybeExperienceLevel ->
            questionContainer
                howExperiencedAreYouWithElmTitle
                (answers PressedHowExperiencedAreYouWithElm experienceLevelToString experienceLevelAnswers maybeExperienceLevel)

        HowExperiencedAreYouWithProgramming maybeExperienceLevel ->
            questionContainer
                howExperiencedAreYouWithProgrammingTitle
                (answers PressedHowExperiencedAreYouWithProgramming experienceLevelToString experienceLevelAnswers maybeExperienceLevel)

        WhatCountryAreYouFrom maybeCountry ->
            questionContainer
                countryQuestionTitle
                (answers PressedWhatCountryAreYouFrom countryToString countryAnswers maybeCountry)


countryToString : Country -> String
countryToString country =
    if country.name == "United Kingdom of Great Britain and Northern Ireland" then
        country.flag ++ " United Kingdom"

    else
        country.flag ++ " " ++ country.name


countryAnswers =
    Countries.all
        |> List.map
            (\country ->
                if country.code == "TW" then
                    { country | name = "Taiwan" }

                else
                    country
            )


experienceLevelAnswers =
    [ Expert, Intermediate, Beginner ]


experienceLevelToString : ExperienceLevel -> String
experienceLevelToString experienceLevel =
    case experienceLevel of
        Expert ->
            "Expert"

        Intermediate ->
            "Intermediate"

        Beginner ->
            "Beginner"


questionContainer : Element msg -> Element msg -> Element msg
questionContainer title answers_ =
    Element.column
        [ Element.spacing 16, Element.centerX, Element.centerY ]
        [ title, answers_ ]


happinessQuestionTitle =
    Element.paragraph [ Element.Font.center ] [ Element.text "How are you doing?" ]


happinessAnswers =
    [ Good, NotGood ]


happinessToString : Happiness -> String
happinessToString howAreYou =
    case howAreYou of
        Good ->
            "👍"

        NotGood ->
            "👎"


answers : (a -> msg) -> (a -> String) -> List a -> Maybe a -> Element msg
answers onPress toString options selected =
    Element.wrappedRow [ Element.spacing 8, Element.centerX, Element.width Element.fill ]
        (List.map
            (\option ->
                let
                    text =
                        toString option
                in
                Element.Input.button
                    [ Element.Background.color
                        (if selected == Just option then
                            Element.rgb 0.7 0.8 0.9

                         else
                            Element.rgb 0.9 0.9 0.9
                        )
                    , Element.height Element.fill
                    , Element.Border.width 1
                    , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                    , Element.padding 16
                    , if String.length text > 30 then
                        Element.Font.size 12

                      else if String.length text > 20 then
                        Element.Font.size 16

                      else
                        Element.Font.size 20
                    ]
                    { onPress = Just (onPress option)
                    , label = Element.text text
                    }
            )
            options
        )
