module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (SessionId)
import Url exposing (Url)


type FrontendModel
    = IsAdmin
    | IsUser Question


type Question
    = HowAreYou (Maybe Happiness)
    | HowExperiencedAreYouWithElm (Maybe ExperienceLevel)
    | HowExperiencedAreYouWithProgramming (Maybe ExperienceLevel)
    | WhatCountryAreYouFrom (Maybe Country)


type Happiness
    = Good
    | NotGood


type ExperienceLevel
    = Expert
    | Intermediate
    | Beginner


type alias BackendModel =
    { howAreYou : Dict SessionId Happiness
    , howExperiencedAreYouWithElm : Dict SessionId ExperienceLevel
    , howExperiencedAreYouWithProgramming : Dict SessionId ExperienceLevel
    , whatCountryAreYouFrom : Dict SessionId Country
    }


type Country
    = Country String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedHowAreYou Happiness
    | PressedHowExperiencedAreYouWithElm ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming ExperienceLevel
    | PressedWhatCountryAreYouFrom Country


type ToBackend
    = ChoseHowAreYou Happiness
    | ChoseHowExperiencedAreYouWithElm ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming ExperienceLevel
    | ChoseWhatCountryAreYouFrom Country
    | AdminRequestNextQuestion


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = UpdateAdmin
        { howAreYou : List Happiness
        , howExperiencedAreYouWithElm : List ExperienceLevel
        , howExperiencedAreYouWithProgramming : List ExperienceLevel
        , whatCountryAreYouFrom : List Country
        }
    | NextQuestion
