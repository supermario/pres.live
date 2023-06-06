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
    = IsAdmin UserModel ViewMode Questions.CurrentQuestion AdminData
    | IsUser UserModel


type ViewMode
    = Admin
    | Present


type alias UserModel =
    { question : Maybe Questions.Question
    , comment : String
    , commentSubmitStatus : SubmitStatus
    , userCount : Int
    }


type SubmitStatus
    = NotSubmitted
    | Submitting


type alias BackendModel =
    { howAreYou : Dict SessionId Questions.Happiness
    , howExperiencedAreYouWithElm : Dict SessionId Questions.ExperienceLevel
    , howExperiencedAreYouWithProgramming : Dict SessionId Questions.ExperienceLevel
    , whatCountryAreYouFrom : Dict SessionId Country
    , attributeQuestionAnswers : Dict SessionId Questions.AttributeQuestionAnswers
    , normalizedQuestionAnswers : Dict SessionId (Dict String (List String))
    , currentQuestion : Questions.CurrentQuestion
    , adminSessions : Set SessionId
    , comments : List Comment
    , bannedUsers : Set SessionId
    , sessions : Set ( SessionId, ClientId )
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedHowAreYou Questions.Happiness
    | PressedHowExperiencedAreYouWithElm Questions.ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming Questions.ExperienceLevel
    | PressedWhatCountryAreYouFrom Country
    | PressedAttributeQuestionAnswer Questions.AttributeQuestionAnswer
    | PressedNormalisedQuestionAnswer String (List String)
    | AdminPressedNextQuestion
    | AdminPressedReset
    | AdminToggledMode
    | TypedComment String
    | PressedSubmitComment
    | PressedBanUser SessionId
    | PressedRemoveAllBans
    | Noop String


type ToBackend
    = ChoseHowAreYou Questions.Happiness
    | ChoseHowExperiencedAreYouWithElm Questions.ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming Questions.ExperienceLevel
    | ChoseWhatCountryAreYouFrom Country
    | PressedAttributeQuestionAnswer_ Questions.AttributeQuestionAnswer
    | PressedNormalisedQuestionAnswer_ String (List String)
    | AdminAuth String
    | AdminRequestNextQuestion
    | AdminRequestReset
    | PostCommentRequest NonemptyString
    | BanUserRequest SessionId
    | RemoveAllBansRequest


type BackendMsg
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId
    | GotTimeForUpdateFromFrontend SessionId ClientId ToBackend Time.Posix


type alias AdminData =
    { howAreYou : List Questions.Happiness
    , howExperiencedAreYouWithElm : List Questions.ExperienceLevel
    , howExperiencedAreYouWithProgramming : List Questions.ExperienceLevel
    , whatCountryAreYouFrom : List Country
    , attributeQuestionAnswers : Dict SessionId Questions.AttributeQuestionAnswers
    , normalizedQuestionAnswers : Dict SessionId (Dict String (List String))
    , comments : List Comment
    }


type ToFrontend
    = SetAdminMode Questions.CurrentQuestion AdminData
    | StreamAttributeQuestionAnswer SessionId Questions.AttributeQuestionAnswer
    | StreamNormalizedQuestionAnswer SessionId String (List String)
    | StreamComment Comment
    | UpdateAdmin AdminData
    | SetCurrentQuestion Questions.CurrentQuestion
    | PostCommentResponse
    | RemoveAllBansResponse (List Comment)
    | UserCountChanged Int


type alias Comment =
    { sessionId : SessionId
    , text : NonemptyString
    , time : Time.Posix
    }
