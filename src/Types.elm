module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Countries exposing (Country)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import String.Nonempty exposing (NonemptyString)
import Time
import Url exposing (Url)


type FrontendModel
    = IsAdmin CurrentQuestion AdminData
    | IsUser UserModel


type alias UserModel =
    { question : Question
    , comment : String
    , commentSubmitStatus : SubmitStatus
    }


type SubmitStatus
    = NotSubmitted
    | Submitting


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
    , comments : List Comment
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
    | TypedComment String
    | PressedSubmitComment


type ToBackend
    = ChoseHowAreYou Happiness
    | ChoseHowExperiencedAreYouWithElm ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming ExperienceLevel
    | ChoseWhatCountryAreYouFrom Country
    | AdminAuth String
    | AdminRequestNextQuestion
    | AdminRequestReset
    | PostCommentRequest NonemptyString


type BackendMsg
    = UserConnected SessionId ClientId
    | GotTimeForUpdateFromFrontend SessionId ClientId ToBackend Time.Posix


type alias AdminData =
    { howAreYou : List Happiness
    , howExperiencedAreYouWithElm : List ExperienceLevel
    , howExperiencedAreYouWithProgramming : List ExperienceLevel
    , whatCountryAreYouFrom : List Country
    , comments : List Comment
    }


type ToFrontend
    = SetAdminMode CurrentQuestion AdminData
    | UpdateAdmin AdminData
    | SetCurrentQuestion CurrentQuestion
    | PostCommentResponse


type alias Comment =
    { sessionId : SessionId
    , text : NonemptyString
    , time : Time.Posix
    }
