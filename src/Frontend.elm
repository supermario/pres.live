module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Countries exposing (Country)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Lamdera
import List.Extra as List
import Questions exposing (..)
import String.Nonempty
import Types exposing (..)
import Ui
import Url
import Url.Parser
import Url.Parser.Query
import View.Comments


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
            ( IsUser userModelInit
            , Cmd.batch
                [ Lamdera.sendToBackend (AdminAuth secret)
                , setNewUrl
                ]
            )

        _ ->
            ( IsUser userModelInit
            , Cmd.batch
                [ setNewUrl
                ]
            )


userModelInit =
    { question = Nothing, comment = "", commentSubmitStatus = NotSubmitted, userCount = 1 }


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
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        Just (HowAreYou _) ->
                            ( { userData | question = Just happiness |> HowAreYou |> Just } |> IsUser
                            , Lamdera.sendToBackend (ChoseHowAreYou happiness)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithElm experienceLevel ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        Just (HowExperiencedAreYouWithElm _) ->
                            ( { userData
                                | question = Just experienceLevel |> HowExperiencedAreYouWithElm |> Just
                              }
                                |> IsUser
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithElm experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithProgramming experienceLevel ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        Just (HowExperiencedAreYouWithProgramming _) ->
                            ( { userData
                                | question = Just experienceLevel |> HowExperiencedAreYouWithProgramming |> Just
                              }
                                |> IsUser
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithProgramming experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedWhatCountryAreYouFrom country ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case userData.question of
                        Just (WhatCountryAreYouFrom _) ->
                            ( { userData | question = Just country |> WhatCountryAreYouFrom |> Just } |> IsUser
                            , Lamdera.sendToBackend (ChoseWhatCountryAreYouFrom country)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedAttributeQuestionAnswer attributeQuestionAnswer ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userModel ->
                    case userModel.question of
                        Just (AttributeQuestion _) ->
                            ( { userModel | question = AttributeQuestion attributeQuestionAnswer |> Just } |> IsUser
                            , Lamdera.sendToBackend (PressedAttributeQuestionAnswer_ attributeQuestionAnswer)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedNormalisedQuestionAnswer title answers ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userModel ->
                    case userModel.question of
                        Just (NormalisedQuestionA question _) ->
                            ( { userModel | question = NormalisedQuestionA question answers |> Just } |> IsUser
                            , Lamdera.sendToBackend (PressedNormalisedQuestionAnswer_ title answers)
                            )

                        _ ->
                            ( model, Cmd.none )

        AdminPressedNextQuestion ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestNextQuestion )

                IsUser _ ->
                    ( model, Cmd.none )

        AdminPressedPreviousQuestion ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Lamdera.sendToBackend AdminPressedPreviousQuestion_ )

                IsUser _ ->
                    ( model, Cmd.none )

        AdminPressedReset ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestReset )

                IsUser _ ->
                    ( model, Cmd.none )

        AdminToggledMode ->
            case model of
                IsAdmin userModel mode q d ->
                    ( IsAdmin userModel
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
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    ( IsUser { userData | comment = text }, Cmd.none )

        PressedSubmitComment ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    case ( userData.commentSubmitStatus, String.Nonempty.fromString userData.comment ) of
                        ( NotSubmitted, Just comment ) ->
                            ( IsUser { userData | commentSubmitStatus = Submitting }
                            , PostCommentRequest comment |> Lamdera.sendToBackend
                            )

                        _ ->
                            ( model, Cmd.none )

        Noop _ ->
            ( model, Cmd.none )

        PressedBanUser sessionId ->
            case model of
                IsAdmin userModel viewMode currentQuestion adminData ->
                    ( IsAdmin
                        userModel
                        viewMode
                        currentQuestion
                        { adminData | comments = List.filter (\comment -> comment.sessionId /= sessionId) adminData.comments }
                    , Lamdera.sendToBackend (BanUserRequest sessionId)
                    )

                IsUser _ ->
                    ( model, Cmd.none )

        PressedRemoveAllBans ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Lamdera.sendToBackend RemoveAllBansRequest )

                IsUser _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SetAdminMode currentQuestion answerData ->
            case model of
                IsAdmin userModel presentMode _ _ ->
                    ( IsAdmin userModel presentMode currentQuestion answerData, Cmd.none )

                _ ->
                    ( IsAdmin userModelInit Admin currentQuestion answerData, Cmd.none )

        StreamAttributeQuestionAnswer sessionId attributeQuestionAnswer ->
            case model of
                IsAdmin userModel presentMode currentQuestion answerData ->
                    ( IsAdmin userModel
                        presentMode
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

        StreamNormalizedQuestionAnswer sessionId title answers ->
            case model of
                IsAdmin userModel presentMode currentQuestion answerData ->
                    ( IsAdmin userModel
                        presentMode
                        currentQuestion
                        { answerData
                            | normalizedQuestionAnswers =
                                answerData.normalizedQuestionAnswers
                                    |> Dict.update sessionId
                                        (\answersDictM ->
                                            case answersDictM of
                                                Just answersDict ->
                                                    Dict.insert title answers answersDict |> Just

                                                Nothing ->
                                                    Dict.singleton title answers |> Just
                                        )
                        }
                    , Cmd.none
                    )

                IsUser _ ->
                    ( model, Cmd.none )

        StreamComment comment ->
            case model of
                IsAdmin userModel presentMode currentQuestion answerData ->
                    ( IsAdmin userModel presentMode currentQuestion { answerData | comments = comment :: answerData.comments }
                    , Cmd.none
                    )

                IsUser _ ->
                    ( model, Cmd.none )

        UpdateAdmin answerData ->
            case model of
                IsAdmin userModel presentMode currentQuestion _ ->
                    ( IsAdmin userModel presentMode currentQuestion answerData, Cmd.none )

                IsUser _ ->
                    ( model, Cmd.none )

        SetCurrentQuestion question ->
            case model of
                IsAdmin userModel mode _ adminData ->
                    ( IsAdmin userModel mode question adminData, Cmd.none )

                IsUser userData ->
                    ( { userData | question = currentPageToQuestion question } |> IsUser, Cmd.none )

        PostCommentResponse ->
            case model of
                IsAdmin _ _ _ _ ->
                    ( model, Cmd.none )

                IsUser userData ->
                    ( { userData | commentSubmitStatus = NotSubmitted, comment = "" } |> IsUser, Cmd.none )

        RemoveAllBansResponse comments ->
            case model of
                IsAdmin userModel viewMode currentQuestion adminData ->
                    ( IsAdmin userModel viewMode currentQuestion { adminData | comments = comments }, Cmd.none )

                IsUser userData ->
                    ( model, Cmd.none )

        UserCountChanged userCount ->
            case model of
                IsAdmin _ viewMode currentQuestion adminData ->
                    ( model, Cmd.none )

                IsUser userData ->
                    ( IsUser { userData | userCount = userCount }, Cmd.none )


currentPageToQuestion : CurrentQuestion -> Maybe Questions.Question
currentPageToQuestion currentQuestion =
    case currentQuestion of
        IntroScreen ->
            Nothing

        HowAreYou_ ->
            HowAreYou Nothing |> Just

        HowExperiencedAreYouWithElm_ ->
            HowExperiencedAreYouWithElm Nothing |> Just

        HowExperiencedAreYouWithProgramming_ ->
            HowExperiencedAreYouWithProgramming Nothing |> Just

        WhatCountryAreYouFrom_ ->
            WhatCountryAreYouFrom Nothing |> Just

        AttributeQuestion_ attributeType ->
            attributeTypeToAttributeQuestionAnswerDefault attributeType |> AttributeQuestion |> Just

        NormalisedQuestion_ normalisedQuestion ->
            NormalisedQuestionA normalisedQuestion [] |> Just


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Hello Lambda Days!"
    , body =
        [ Ui.layout
            (case model of
                IsAdmin userModel presentMode currentQuestion answerData ->
                    case presentMode of
                        Admin ->
                            column
                                [ width fill, height fill, spacing 20 ]
                                [ adminQuestionView userModel presentMode currentQuestion answerData
                                , row [ spacing 8 ]
                                    [ Ui.button []
                                        AdminToggledMode
                                        (text "Present")
                                    , Ui.button []
                                        AdminPressedPreviousQuestion
                                        (text "Previous Question")
                                    , Ui.button []
                                        AdminPressedNextQuestion
                                        (text "Next Question")
                                    , Ui.button []
                                        AdminPressedReset
                                        (text "Reset Questions")
                                    ]
                                , View.Comments.adminView answerData.comments
                                ]

                        Present ->
                            column
                                [ width fill, height fill, spacing 8, Font.size 80 ]
                                [ adminQuestionView userModel presentMode currentQuestion answerData
                                ]

                IsUser userData ->
                    column
                        [ width fill, height fill, spacing 32 ]
                        [ questionView userData.userCount userData.question
                        , column
                            [ width fill, spacing 16, alignBottom ]
                            [ row [ width fill ]
                                [ Ui.multilineInput "" "Comments & feedback..." TypedComment userData.comment
                                , Ui.button
                                    [ Background.color Ui.colors.green
                                    , height (px 48)
                                    ]
                                    PressedSubmitComment
                                    ((case userData.commentSubmitStatus of
                                        NotSubmitted ->
                                            ">"

                                        Submitting ->
                                            "..."
                                     )
                                        |> text
                                        |> el [ centerX ]
                                    )
                                ]
                            ]
                        ]
            )
        ]
    }


largeQuestionSlide title =
    -- column [ width fill, spacing 60, height fill ]
    --     [ row [ Font.bold, width fill, centerY ] [ paragraph [ Font.center ] [ text nQuestion.title ] ]
    --     , answerOptions
    --     ]
    text title


answerCounterPill : { presentMode : ViewMode, label : String, emoji : String, hideLabelInButton : Bool, count : Int } -> Element msg
answerCounterPill { presentMode, label, emoji, hideLabelInButton, count } =
    [ text emoji
    , if presentMode == Present && hideLabelInButton then
        none

      else
        text label
    , el [ Font.bold, Font.color Ui.colours.bg, Ui.rounded, paddingXY 10 5, Background.color Ui.colours.fg ] <| text <| String.fromInt count
    ]
        |> row [ spacing 10 ]
        |> el
            [ Background.color Ui.colors.bg2
            , Ui.rounded
            , paddingXY 10 5
            ]


adminQuestionView : UserModel -> ViewMode -> CurrentQuestion -> AdminData -> Element FrontendMsg
adminQuestionView userModel mode currentQuestion adminData =
    case currentQuestion of
        IntroScreen ->
            column [ width fill, height fill, spacing 80 ]
                [ paragraph [ Font.color Ui.colours.slide.purple, Font.bold, Font.center, Font.size 80, centerY ] [ text "The unbearable weight of glue 🍯" ]
                , column [ centerX ]
                    [ paragraph [ Font.bold, Font.center, Font.size 60, centerY ] [ text "This talk involves live interaction," ]
                    , paragraph [ Font.bold, Font.center, Font.size 60, centerY ] [ text "open URL on a mobile/tablet/laptop:" ]
                    ]
                , paragraph
                    [ Font.color Ui.colours.slide.purple, Font.bold, Font.underline, Font.center, Font.size 80, centerY ]
                    [ text "pres.lamdera.app" ]
                , row
                    [ centerX ]
                    [ el [ Font.size 80, Font.heavy ] (text (String.fromInt userModel.userCount ++ " "))
                    , el [ Font.size 60 ]
                        (text
                            (Ui.pluralise "person has" "people have" userModel.userCount
                                ++ " joined"
                            )
                        )
                    ]
                ]

        HowAreYou_ ->
            questionContainer
                happinessQuestionTitle
                (adminAnswers mode happinessToString happinessAnswers adminData.howAreYou)

        HowExperiencedAreYouWithElm_ ->
            questionContainer
                howExperiencedAreYouWithElmTitle
                (adminAnswers mode experienceLevelToString experienceLevelAnswers adminData.howExperiencedAreYouWithElm)

        HowExperiencedAreYouWithProgramming_ ->
            questionContainer
                howExperiencedAreYouWithProgrammingTitle
                (adminAnswers mode experienceLevelToString experienceLevelAnswers adminData.howExperiencedAreYouWithProgramming)

        WhatCountryAreYouFrom_ ->
            questionContainer
                countryQuestionTitle
                (adminAnswers mode countryToString countryAnswers adminData.whatCountryAreYouFrom)

        NormalisedQuestion_ nQuestion ->
            let
                allAnswers =
                    adminData.normalizedQuestionAnswers
                        |> Dict.values
                        |> List.filterMap (Dict.get nQuestion.title)
                        |> List.concat

                answerOptions =
                    List.map
                        (\option ->
                            let
                                count =
                                    allAnswers |> List.count ((==) option.emoji)
                            in
                            answerCounterPill
                                { presentMode = mode
                                , label = option.text
                                , emoji = option.emoji
                                , hideLabelInButton = nQuestion.hideLabelInButton
                                , count = count
                                }
                        )
                        nQuestion.options
                        |> wrappedRow [ spacing 8, centerX, centerY ]
            in
            column [ width fill, spacing 60, height fill ]
                [ row [ Font.bold, width fill, centerY ] [ paragraph [ Font.center ] [ text nQuestion.title ] ]
                , answerOptions
                ]

        AttributeQuestion_ attributeType ->
            let
                attributeQuestion =
                    Questions.attributeQuestion attributeType

                answerOptions =
                    List.map
                        (\option ->
                            let
                                answers_ =
                                    Questions.attributeQuestionAnswersForAttributeType attributeType adminData.attributeQuestionAnswers

                                count =
                                    List.count ((==) option.text) answers_
                            in
                            { button =
                                answerCounterPill
                                    { presentMode = mode
                                    , label = option.text
                                    , emoji = option.emoji
                                    , hideLabelInButton = attributeQuestion.hideLabelInButton
                                    , count = count
                                    }
                            , count = count
                            }
                        )
                        attributeQuestion.options
                        |> (\unsortedOptions ->
                                if attributeQuestion.sortResultsByCount then
                                    unsortedOptions
                                        |> List.sortBy (\option -> option.count)
                                        |> List.reverse
                                        |> List.map .button

                                else
                                    unsortedOptions
                                        |> List.map .button
                           )
                        |> wrappedRow [ spacing 20, centerX, centerY ]
            in
            column [ width fill, spacing 60, height fill ]
                [ row [ Font.bold, Font.center, width fill, centerY ] [ paragraph [] [ text attributeQuestion.title ] ]
                , answerOptions
                ]


howExperiencedAreYouWithElmTitle : Element msg
howExperiencedAreYouWithElmTitle =
    paragraph [ Font.center ] [ text "How good are you with Elm?" ]


howExperiencedAreYouWithProgrammingTitle : Element msg
howExperiencedAreYouWithProgrammingTitle =
    paragraph [ Font.center ] [ text "How good are you at programming in general?" ]


countryQuestionTitle : Element msg
countryQuestionTitle =
    paragraph [ Font.center ] [ text "What country do you live in?" ]


adminAnswers : ViewMode -> (a -> String) -> List a -> List a -> Element msg
adminAnswers viewMode toString possibleAnswers answers_ =
    List.filterMap
        (\answer ->
            let
                count =
                    List.count ((==) answer) answers_
            in
            answerCounterPill
                { presentMode = viewMode
                , label = toString answer
                , emoji = ""
                , hideLabelInButton = False
                , count = count
                }
                |> Just
        )
        possibleAnswers
        |> wrappedRow [ spacing 8, centerX, centerY ]


questionView : Int -> Maybe Questions.Question -> Element FrontendMsg
questionView userCount q =
    case q of
        Nothing ->
            column
                [ spacing 32, centerX ]
                [ paragraph
                    [ Font.size 32, Font.heavy, Font.color (rgb255 102 113 208), Font.center ]
                    [ text "The unbearable weight of glue" ]
                , row
                    [ centerX ]
                    [ el [ Font.size 48, Font.heavy ] (text (String.fromInt userCount ++ " "))
                    , el [ Font.size 24 ]
                        (text
                            (Ui.pluralise "person has" "people have" userCount
                                ++ " joined"
                            )
                        )
                    ]
                ]

        Just (HowAreYou maybeHappiness) ->
            questionContainer
                happinessQuestionTitle
                (viewAnswers PressedHowAreYou happinessToString happinessAnswers maybeHappiness)

        Just (HowExperiencedAreYouWithElm maybeExperienceLevel) ->
            questionContainer
                howExperiencedAreYouWithElmTitle
                (viewAnswers PressedHowExperiencedAreYouWithElm experienceLevelToString experienceLevelAnswers maybeExperienceLevel)

        Just (HowExperiencedAreYouWithProgramming maybeExperienceLevel) ->
            questionContainer
                howExperiencedAreYouWithProgrammingTitle
                (viewAnswers PressedHowExperiencedAreYouWithProgramming experienceLevelToString experienceLevelAnswers maybeExperienceLevel)

        Just (WhatCountryAreYouFrom maybeCountry) ->
            questionContainer
                countryQuestionTitle
                (viewAnswers PressedWhatCountryAreYouFrom countryToString countryAnswers maybeCountry)

        Just (NormalisedQuestionA nQuestion userAnswers) ->
            column
                [ spacing 16, centerX, centerY ]
                [ paragraph [ Font.bold, Font.center ] [ text nQuestion.title ]
                , wrappedRow [ spacing 8, centerX ]
                    (List.map
                        (\option ->
                            let
                                label =
                                    option.emoji ++ " " ++ option.text
                            in
                            Ui.button
                                [ Ui.hilightWhen (userAnswers |> List.member option.emoji)
                                , height fill
                                , if String.length label > 30 then
                                    Font.size 12

                                  else if String.length label > 20 then
                                    Font.size 16

                                  else
                                    Font.size 20
                                ]
                                (if nQuestion.multiselect then
                                    PressedNormalisedQuestionAnswer nQuestion.title (userAnswers |> listToggleValue option.emoji)

                                 else
                                    -- Not multiselect, choice replaces answers
                                    PressedNormalisedQuestionAnswer nQuestion.title [ option.emoji ]
                                )
                                (text label)
                        )
                        nQuestion.options
                    )
                ]

        Just (AttributeQuestion attributeQuestionAnswer) ->
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
            column
                [ spacing 16, centerX, centerY ]
                [ paragraph [ Font.center, Font.bold ] [ text attributeQuestion.title ]
                , wrappedRow [ spacing 8, centerX ]
                    (List.map
                        (\option ->
                            let
                                label =
                                    option.emoji ++ " " ++ option.text
                            in
                            Ui.button
                                [ Ui.hilightWhen (isSelectedOption option.text)
                                , height fill
                                , if String.length label > 30 then
                                    Font.size 12

                                  else if String.length label > 20 then
                                    Font.size 16

                                  else
                                    Font.size 20
                                ]
                                (onPress option.text)
                                (text label)
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
    column
        [ spacing 70, width fill, height fill ]
        [ el [ Font.bold, centerX, centerY ] <| title, answers_ ]


happinessQuestionTitle =
    paragraph [ Font.center ] [ text "How are you doing?" ]


happinessAnswers =
    [ Good, NotGood ]


happinessToString : Happiness -> String
happinessToString howAreYou =
    case howAreYou of
        Good ->
            "👍"

        NotGood ->
            "👎"


viewAnswers : (a -> msg) -> (a -> String) -> List a -> Maybe a -> Element msg
viewAnswers onPress toString options selected =
    wrappedRow [ spacing 8, centerX ]
        (List.map
            (\option ->
                let
                    label =
                        toString option
                in
                Ui.button
                    [ Ui.hilightWhen (selected == Just option)
                    , height fill
                    , if String.length label > 30 then
                        Font.size 12

                      else if String.length label > 20 then
                        Font.size 16

                      else
                        Font.size 20
                    ]
                    (onPress option)
                    (text label)
            )
            options
        )
