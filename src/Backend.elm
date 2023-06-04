module Backend exposing (..)

import Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Questions exposing (..)
import Set
import Task
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Lamdera.onConnect UserConnected
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { adminSessions = Set.empty
      , howAreYou = Dict.empty
      , howExperiencedAreYouWithElm = Dict.empty
      , howExperiencedAreYouWithProgramming = Dict.empty
      , whatCountryAreYouFrom = Dict.empty
      , attributeQuestionAnswers = Dict.empty
      , currentQuestion = firstQuestion
      , comments = []
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        UserConnected sessionId clientId ->
            ( model
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (SetCurrentQuestion model.currentQuestion)
                , if model.adminSessions |> Set.member sessionId then
                    Lamdera.sendToFrontend sessionId (SetAdminMode model.currentQuestion (convertModelToAdminUpdate model))

                  else
                    Cmd.none
                ]
            )

        GotTimeForUpdateFromFrontend sessionId clientId toBackend time ->
            updateFromFrontendWithTime time sessionId clientId toBackend model


convertModelToAdminUpdate : BackendModel -> AdminData
convertModelToAdminUpdate model =
    { howAreYou = Dict.values model.howAreYou
    , howExperiencedAreYouWithElm = Dict.values model.howExperiencedAreYouWithElm
    , howExperiencedAreYouWithProgramming = Dict.values model.howExperiencedAreYouWithProgramming
    , whatCountryAreYouFrom = Dict.values model.whatCountryAreYouFrom
    , attributeQuestionAnswers = model.attributeQuestionAnswers
    , comments = model.comments
    }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    ( model, Time.now |> Task.perform (GotTimeForUpdateFromFrontend sessionId clientId msg) )


updateFromFrontendWithTime : Time.Posix -> SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontendWithTime time sessionId clientId msg model =
    let
        updatedAdmins newModel =
            model.adminSessions
                |> Set.toList
                |> List.map
                    (\adminSessionId ->
                        Lamdera.sendToFrontend adminSessionId
                            (UpdateAdmin <| convertModelToAdminUpdate newModel)
                    )
                |> Cmd.batch

        sendToAdmins msg_ =
            model.adminSessions
                |> Set.toList
                |> List.map
                    (\adminSessionId ->
                        Lamdera.sendToFrontend adminSessionId msg_
                    )
                |> Cmd.batch
    in
    case msg of
        ChoseHowAreYou happiness ->
            let
                newModel =
                    { model | howAreYou = Dict.insert sessionId happiness model.howAreYou }
            in
            ( newModel, updatedAdmins newModel )

        ChoseHowExperiencedAreYouWithElm experienceLevel ->
            let
                newModel =
                    { model | howExperiencedAreYouWithElm = Dict.insert sessionId experienceLevel model.howExperiencedAreYouWithElm }
            in
            ( newModel, updatedAdmins newModel )

        ChoseHowExperiencedAreYouWithProgramming experienceLevel ->
            let
                newModel =
                    { model
                        | howExperiencedAreYouWithProgramming =
                            Dict.insert sessionId experienceLevel model.howExperiencedAreYouWithProgramming
                    }
            in
            ( newModel, updatedAdmins newModel )

        ChoseWhatCountryAreYouFrom country ->
            let
                newModel =
                    { model | whatCountryAreYouFrom = Dict.insert sessionId country model.whatCountryAreYouFrom }
            in
            ( newModel, updatedAdmins newModel )

        PressedAttributeQuestionAnswer_ attributeQuestionAnswer ->
            ( { model
                | attributeQuestionAnswers =
                    model.attributeQuestionAnswers
                        |> Dict.update sessionId
                            (\answersM ->
                                answersM
                                    |> Maybe.withDefault Questions.emptyAttributeAnswers
                                    |> Questions.updateAnswers attributeQuestionAnswer
                                    |> Just
                            )
              }
            , sendToAdmins (StreamAttributeQuestionAnswer sessionId attributeQuestionAnswer)
            )

        AdminAuth secret ->
            if secret == Env.secret then
                ( { model | adminSessions = Set.insert sessionId model.adminSessions }
                , Lamdera.sendToFrontend sessionId (SetAdminMode model.currentQuestion (convertModelToAdminUpdate model))
                )

            else
                ( model, Cmd.none )

        AdminRequestNextQuestion ->
            requiringAdmin model
                sessionId
                (\_ ->
                    ( { model | currentQuestion = nextCurrentQuestion model.currentQuestion }
                    , Lamdera.broadcast (SetCurrentQuestion (nextCurrentQuestion model.currentQuestion))
                    )
                )

        AdminRequestReset ->
            requiringAdmin model
                sessionId
                (\_ ->
                    let
                        newModel =
                            { model
                                | howAreYou = Dict.empty
                                , howExperiencedAreYouWithElm = Dict.empty
                                , howExperiencedAreYouWithProgramming = Dict.empty
                                , whatCountryAreYouFrom = Dict.empty
                                , currentQuestion = firstQuestion
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ Lamdera.broadcast (SetCurrentQuestion newModel.currentQuestion)
                        , Lamdera.sendToFrontend sessionId (SetAdminMode newModel.currentQuestion (convertModelToAdminUpdate newModel))
                        ]
                    )
                )

        PostCommentRequest comment ->
            ( { model | comments = { text = comment, time = time, sessionId = sessionId } :: model.comments }
            , Lamdera.sendToFrontend clientId PostCommentResponse
            )


requiringAdmin model sessionId fn =
    if model.adminSessions |> Set.member sessionId then
        fn ()

    else
        ( model, Cmd.none )


allQuestions =
    [ AttributeQuestion_ AttendanceReason
    , AttributeQuestion_ Profession
    , AttributeQuestion_ Experience
    , AttributeQuestion_ Scale
    , AttributeQuestion_ Languages
    , HowAreYou_
    , HowExperiencedAreYouWithElm_
    , HowExperiencedAreYouWithProgramming_
    , WhatCountryAreYouFrom_
    ]


firstQuestion =
    allQuestions |> List.head |> Maybe.withDefault HowAreYou_


nextCurrentQuestion : CurrentQuestion -> CurrentQuestion
nextCurrentQuestion currentQuestion =
    case List.find (\q -> q == currentQuestion) allQuestions of
        Just _ ->
            case List.drop 1 (List.dropWhile (\q -> q /= currentQuestion) allQuestions) of
                nextQuestion :: _ ->
                    nextQuestion

                [] ->
                    currentQuestion

        Nothing ->
            firstQuestion
