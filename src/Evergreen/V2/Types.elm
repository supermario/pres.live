module Evergreen.V2.Types exposing (..)

import Browser
import Countries
import Dict
import Lamdera
import Url


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


type alias AdminData =
    { howAreYou : List Happiness
    , howExperiencedAreYouWithElm : List ExperienceLevel
    , howExperiencedAreYouWithProgramming : List ExperienceLevel
    , whatCountryAreYouFrom : List Countries.Country
    }


type Question
    = HowAreYou (Maybe Happiness)
    | HowExperiencedAreYouWithElm (Maybe ExperienceLevel)
    | HowExperiencedAreYouWithProgramming (Maybe ExperienceLevel)
    | WhatCountryAreYouFrom (Maybe Countries.Country)


type FrontendModel
    = IsAdmin CurrentQuestion AdminData
    | IsUser Question


type alias BackendModel =
    { howAreYou : Dict.Dict Lamdera.SessionId Happiness
    , howExperiencedAreYouWithElm : Dict.Dict Lamdera.SessionId ExperienceLevel
    , howExperiencedAreYouWithProgramming : Dict.Dict Lamdera.SessionId ExperienceLevel
    , whatCountryAreYouFrom : Dict.Dict Lamdera.SessionId Countries.Country
    , currentQuestion : CurrentQuestion
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedHowAreYou Happiness
    | PressedHowExperiencedAreYouWithElm ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming ExperienceLevel
    | PressedWhatCountryAreYouFrom Countries.Country
    | AdminPressedNextQuestion
    | AdminPressedReset


type ToBackend
    = ChoseHowAreYou Happiness
    | ChoseHowExperiencedAreYouWithElm ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming ExperienceLevel
    | ChoseWhatCountryAreYouFrom Countries.Country
    | AdminRequestNextQuestion
    | AdminRequestReset


type BackendMsg
    = UserConnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = UpdateAdmin AdminData
    | SetCurrentQuestion CurrentQuestion
