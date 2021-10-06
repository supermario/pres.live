module Backend exposing (..)

import Dict
import Html
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { howAreYou = Dict.empty
      , howExperiencedAreYouWithElm = Dict.empty
      , howExperiencedAreYouWithProgramming = Dict.empty
      , whatCountryAreYouFrom = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


convertModelToAdminUpdate model =
    { howAreYou = Dict.values model.howAreYou
    , howExperiencedAreYouWithElm = Dict.values model.howExperiencedAreYouWithElm
    , howExperiencedAreYouWithProgramming = Dict.values model.howExperiencedAreYouWithProgramming
    , whatCountryAreYouFrom = Dict.values model.whatCountryAreYouFrom
    }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId _ msg model =
    case msg of
        ChoseHowAreYou happiness ->
            let
                newModel =
                    { model | howAreYou = Dict.insert sessionId happiness model.howAreYou }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        ChoseHowExperiencedAreYouWithElm experienceLevel ->
            let
                newModel =
                    { model | howExperiencedAreYouWithElm = Dict.insert sessionId experienceLevel model.howExperiencedAreYouWithElm }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        ChoseHowExperiencedAreYouWithProgramming experienceLevel ->
            let
                newModel =
                    { model | howExperiencedAreYouWithProgramming = Dict.insert sessionId experienceLevel model.howExperiencedAreYouWithProgramming }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        ChoseWhatCountryAreYouFrom country ->
            let
                newModel =
                    { model | whatCountryAreYouFrom = Dict.insert sessionId country model.whatCountryAreYouFrom }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        AdminRequestNextQuestion ->
            Debug.todo ""
