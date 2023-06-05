module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Countries exposing (Country)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Questions
import Set exposing (Set)
import String.Nonempty exposing (NonemptyString)
import Time
import Url exposing (Url)


type FrontendModel
    = IsAdmin ViewMode CurrentQuestion AdminData
    | IsUser UserModel


type ViewMode
    = Admin
    | Present


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
    | AttributeQuestion Questions.AttributeQuestionAnswer


type CurrentQuestion
    = HowAreYou_
    | HowExperiencedAreYouWithElm_
    | HowExperiencedAreYouWithProgramming_
    | WhatCountryAreYouFrom_
    | AttributeQuestion_ Questions.AttributeType


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
    , attributeQuestionAnswers : Dict SessionId Questions.AttributeQuestionAnswers
    , currentQuestion : CurrentQuestion
    , adminSessions : Set SessionId
    , comments : List Comment
    , bannedUsers : Set SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedHowAreYou Happiness
    | PressedHowExperiencedAreYouWithElm ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming ExperienceLevel
    | PressedWhatCountryAreYouFrom Country
    | PressedAttributeQuestionAnswer Questions.AttributeQuestionAnswer
    | AdminPressedNextQuestion
    | AdminPressedReset
    | AdminToggledMode
    | TypedComment String
    | PressedSubmitComment
    | PressedBanUser SessionId
    | PressedRemoveAllBans
    | Noop String


type ToBackend
    = ChoseHowAreYou Happiness
    | ChoseHowExperiencedAreYouWithElm ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming ExperienceLevel
    | ChoseWhatCountryAreYouFrom Country
    | PressedAttributeQuestionAnswer_ Questions.AttributeQuestionAnswer
    | AdminAuth String
    | AdminRequestNextQuestion
    | AdminRequestReset
    | PostCommentRequest NonemptyString
    | BanUserRequest SessionId
    | RemoveAllBansRequest


type BackendMsg
    = UserConnected SessionId ClientId
    | GotTimeForUpdateFromFrontend SessionId ClientId ToBackend Time.Posix


type alias AdminData =
    { howAreYou : List Happiness
    , howExperiencedAreYouWithElm : List ExperienceLevel
    , howExperiencedAreYouWithProgramming : List ExperienceLevel
    , whatCountryAreYouFrom : List Country
    , attributeQuestionAnswers : Dict SessionId Questions.AttributeQuestionAnswers
    , comments : List Comment
    }


type ToFrontend
    = SetAdminMode CurrentQuestion AdminData
    | StreamAttributeQuestionAnswer SessionId Questions.AttributeQuestionAnswer
    | StreamComment Comment
    | UpdateAdmin AdminData
    | SetCurrentQuestion CurrentQuestion
    | PostCommentResponse
    | RemoveAllBansResponse (List Comment)


type alias Comment =
    { sessionId : SessionId
    , text : NonemptyString
    , time : Time.Posix
    }
