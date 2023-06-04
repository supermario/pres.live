module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Countries exposing (Country)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)


type FrontendModel
    = IsAdmin CurrentQuestion AdminData
    | IsUser Question


type Question
    = HowAreYou (Maybe Happiness)
    | HowExperiencedAreYouWithElm (Maybe ExperienceLevel)
    | HowExperiencedAreYouWithProgramming (Maybe ExperienceLevel)
    | WhatCountryAreYouFrom (Maybe Country)


type CurrentQuestion
    = HowAreYou_
    | HowExperiencedAreYouWithElm_
    | HowExperiencedAreYouWithProgramming_
    | WhatCountryAreYouFrom_


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
    , currentQuestion : CurrentQuestion
    , adminSessions : Set SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedHowAreYou Happiness
    | PressedHowExperiencedAreYouWithElm ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming ExperienceLevel
    | PressedWhatCountryAreYouFrom Country
    | AdminPressedNextQuestion
    | AdminPressedReset


type ToBackend
    = ChoseHowAreYou Happiness
    | ChoseHowExperiencedAreYouWithElm ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming ExperienceLevel
    | ChoseWhatCountryAreYouFrom Country
    | AdminAuth String
    | AdminRequestNextQuestion
    | AdminRequestReset


type BackendMsg
    = UserConnected SessionId ClientId


type alias AdminData =
    { howAreYou : List Happiness
    , howExperiencedAreYouWithElm : List ExperienceLevel
    , howExperiencedAreYouWithProgramming : List ExperienceLevel
    , whatCountryAreYouFrom : List Country
    }


type ToFrontend
    = SetAdminMode CurrentQuestion AdminData
    | UpdateAdmin AdminData
    | SetCurrentQuestion CurrentQuestion
