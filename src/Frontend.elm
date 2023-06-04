module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Countries exposing (Country)
import Dict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes as Attr
import Lamdera
import List.Extra as List
import Questions exposing (..)
import String.Nonempty
import Types exposing (..)
import Ui
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
            ( IsUser { question = HowAreYou Nothing, comment = "", commentSubmitStatus = NotSubmitted }
            , Cmd.batch
                [ Lamdera.sendToBackend (AdminAuth secret)
                , setNewUrl
                ]
            )

        _ ->
            ( IsUser { question = HowAreYou Nothing, comment = "", commentSubmitStatus = NotSubmitted }
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
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        HowAreYou _ ->
                            ( { userData | question = Just happiness |> HowAreYou } |> IsUser
                            , Lamdera.sendToBackend (ChoseHowAreYou happiness)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithElm experienceLevel ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        HowExperiencedAreYouWithElm _ ->
                            ( { userData | question = Just experienceLevel |> HowExperiencedAreYouWithElm } |> IsUser
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithElm experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithProgramming experienceLevel ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        HowExperiencedAreYouWithProgramming _ ->
                            ( { userData | question = Just experienceLevel |> HowExperiencedAreYouWithProgramming } |> IsUser
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithProgramming experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedWhatCountryAreYouFrom country ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        WhatCountryAreYouFrom _ ->
                            ( { userData | question = Just country |> WhatCountryAreYouFrom } |> IsUser
                            , Lamdera.sendToBackend (ChoseWhatCountryAreYouFrom country)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedAttributeQuestionAnswer attributeQuestionAnswer ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userModel ->
                    case userModel.question of
                        AttributeQuestion _ ->
                            ( { userModel | question = AttributeQuestion attributeQuestionAnswer } |> IsUser
                            , Lamdera.sendToBackend (PressedAttributeQuestionAnswer_ attributeQuestionAnswer)
                            )

                        _ ->
                            ( model, Cmd.none )

        AdminPressedNextQuestion ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestNextQuestion )

                IsUser _ ->
                    ( model, Cmd.none )

        AdminPressedReset ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestReset )

                IsUser _ ->
                    ( model, Cmd.none )

        AdminToggledMode ->
            case model of
                IsAdmin mode q d ->
                    ( IsAdmin
                        (case mode of
                            Admin ->
                                Present

                            Present ->
                                Admin
                        )
                        q
                        d
                    , Cmd.none
                    )

                IsUser _ ->
                    ( model, Cmd.none )

        TypedComment text ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    ( IsUser { userData | comment = text }, Cmd.none )

        PressedSubmitComment ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case ( userData.commentSubmitStatus, String.Nonempty.fromString userData.comment ) of
                        ( NotSubmitted, Just comment ) ->
                            ( IsUser { userData | commentSubmitStatus = Submitting }
                            , PostCommentRequest comment |> Lamdera.sendToBackend
                            )

                        _ ->
                            ( model, Cmd.none )

        Noop string ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SetAdminMode currentQuestion answerData ->
            ( IsAdmin Admin currentQuestion answerData, Cmd.none )

        StreamAttributeQuestionAnswer sessionId attributeQuestionAnswer ->
            case model of
                IsAdmin presentMode currentQuestion answerData ->
                    ( IsAdmin presentMode
                        currentQuestion
                        { answerData
                            | attributeQuestionAnswers =
                                answerData.attributeQuestionAnswers
                                    |> Dict.update sessionId
                                        (\answersM ->
                                            answersM
                                                |> Maybe.withDefault Questions.emptyAttributeAnswers
                                                |> Questions.updateAnswers attributeQuestionAnswer
                                                |> Just
                                        )
                        }
                    , Cmd.none
                    )

                IsUser _ ->
                    ( model, Cmd.none )

        StreamComment comment ->
            case model of
                IsAdmin presentMode currentQuestion answerData ->
                    ( IsAdmin presentMode currentQuestion { answerData | comments = comment :: answerData.comments }
                    , Cmd.none
                    )

                IsUser _ ->
                    ( model, Cmd.none )

        UpdateAdmin answerData ->
            case model of
                IsAdmin presentMode currentQuestion _ ->
                    ( IsAdmin presentMode currentQuestion answerData, Cmd.none )

                IsUser _ ->
                    ( model, Cmd.none )

        SetCurrentQuestion question ->
            case model of
                IsAdmin mode _ adminData ->
                    ( IsAdmin mode question adminData, Cmd.none )

                IsUser userData ->
                    ( { userData | question = currentQuestionToQuestion question } |> IsUser, Cmd.none )

        PostCommentResponse ->
            case model of
                IsAdmin _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    ( { userData | commentSubmitStatus = NotSubmitted, comment = "" } |> IsUser, Cmd.none )


currentQuestionToQuestion : CurrentQuestion -> Types.Question
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

        AttributeQuestion_ attributeType ->
            AttributeQuestion <| attributeTypeToAttributeQuestionAnswerDefault attributeType


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Hello Lambda Days!"
    , body =
        [ Element.layout
            [ Element.padding 16 ]
            (case model of
                IsAdmin presentMode currentQuestion answerData ->
                    case presentMode of
                        Admin ->
                            Element.column
                                [ Element.width Element.fill, Element.height Element.fill, Element.spacing 8 ]
                                [ adminQuestionView currentQuestion answerData
                                , Ui.button_
                                    AdminPressedNextQuestion
                                    (Element.text "Next Question")
                                , Ui.button_
                                    AdminPressedReset
                                    (Element.text "Reset Questions")
                                , Ui.button_
                                    AdminToggledMode
                                    (Element.text "Present Mode")
                                , answerData.comments |> List.take 10 |> List.map commentView |> Element.column [ Element.width Element.fill ]
                                ]

                        Present ->
                            Element.column
                                [ Element.width Element.fill, Element.height Element.fill, Element.spacing 8 ]
                                [ adminQuestionView currentQuestion answerData
                                ]

                IsUser userData ->
                    Element.column
                        [ Element.width Element.fill, Element.spacing 32 ]
                        [ questionView userData.question
                        , Element.column
                            [ Element.width Element.fill, Element.spacing 16 ]
                            [ Element.Input.multiline
                                [ Attr.attribute "data-gramm_editor" "false" |> Element.htmlAttribute
                                , Attr.attribute "data-enable-grammarly" "false" |> Element.htmlAttribute
                                ]
                                { text = userData.comment
                                , placeholder = Nothing
                                , onChange = TypedComment
                                , label =
                                    Element.Input.labelAbove
                                        []
                                        (Element.paragraph [] [ Element.text "Have any questions or comments?" ])
                                , spellcheck = True
                                }
                            , Ui.button
                                [ Element.Background.color (Element.rgb 0.8 0.9 0.8)
                                , Element.paddingXY 16 8
                                , Element.width Element.fill
                                ]
                                PressedSubmitComment
                                ((case userData.commentSubmitStatus of
                                    NotSubmitted ->
                                        "Submit question/comment"

                                    Submitting ->
                                        "Submitting..."
                                 )
                                    |> Element.text
                                    |> Element.el [ Element.centerX ]
                                )
                            ]
                        ]
            )
        ]
    }


commentView : Comment -> Element FrontendMsg
commentView comment =
    Element.column
        [ Element.width Element.fill, Element.spacing 16 ]
        [ Element.paragraph []
            [ comment.text
                |> String.Nonempty.toString
                |> Element.text
            ]
        ]


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

        AttributeQuestion_ attributeType ->
            let
                attributeQuestion =
                    Questions.attributeQuestion attributeType
            in
            List.map
                (\option ->
                    let
                        answers_ =
                            Questions.attributeQuestionAnswersForAttributeType attributeType adminData.attributeQuestionAnswers

                        count =
                            List.count ((==) option.text) answers_
                    in
                    -- if count == 0 then
                    --     Nothing
                    -- else
                    option.emoji
                        ++ " "
                        ++ option.text
                        ++ " "
                        ++ String.fromInt count
                        |> Element.text
                        |> Element.el
                            [ Element.Background.color <| Element.rgb 0.9 0.9 0.9
                            , Element.Border.width 1
                            , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                            , Element.padding 16
                            ]
                )
                attributeQuestion.options
                |> Element.wrappedRow [ Element.spacing 8, Element.centerX ]


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


questionView : Types.Question -> Element FrontendMsg
questionView q =
    case q of
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

        AttributeQuestion attributeQuestionAnswer ->
            let
                attributeType =
                    case attributeQuestionAnswer of
                        AttendanceReasonAnswer _ ->
                            AttendanceReason

                        ProfessionAnswer _ ->
                            Profession

                        ExperienceAnswer _ ->
                            Experience

                        ScaleAnswer _ ->
                            Scale

                        LanguagesAnswer _ ->
                            Languages

                attributeQuestion =
                    Questions.attributeQuestion attributeType

                isSelectedOption : String -> Bool
                isSelectedOption optionText =
                    case attributeQuestionAnswer of
                        AttendanceReasonAnswer answer ->
                            answer |> Maybe.map (List.map attendanceReasonToString >> List.member optionText) |> Maybe.withDefault False

                        ProfessionAnswer answer ->
                            answer |> Maybe.map (List.map professionToString >> List.member optionText) |> Maybe.withDefault False

                        ExperienceAnswer answer ->
                            answer |> Maybe.map (experienceToString >> (==) optionText) |> Maybe.withDefault False

                        ScaleAnswer answer ->
                            answer |> Maybe.map (scaleToString >> (==) optionText) |> Maybe.withDefault False

                        LanguagesAnswer answer ->
                            answer |> Maybe.map (List.map languageToString >> List.member optionText) |> Maybe.withDefault False

                onPress optionText =
                    case Questions.optionTextToAttributeQuestionAnswer attributeQuestionAnswer optionText of
                        Just attendanceReasonAnswer ->
                            PressedAttributeQuestionAnswer attendanceReasonAnswer

                        Nothing ->
                            -- Should be impossible
                            Noop <| "impossible onPress in questionView: " ++ optionText
            in
            Element.column
                [ Element.spacing 16, Element.centerX, Element.centerY ]
                [ Element.text attributeQuestion.title
                , Element.wrappedRow [ Element.spacing 8, Element.centerX, Element.width Element.fill ]
                    (List.map
                        (\option ->
                            let
                                text =
                                    option.emoji ++ " " ++ option.text
                            in
                            Element.Input.button
                                [ Element.Background.color
                                    (if isSelectedOption option.text then
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
                                { onPress = Just (onPress option.text)
                                , label = Element.text text
                                }
                        )
                        attributeQuestion.options
                    )
                ]


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
                Ui.button
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
                    (onPress option)
                    (Element.text text)
            )
            options
        )
