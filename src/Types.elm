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
    = IsAdmin ViewMode Questions.CurrentQuestion AdminData
    | IsUser UserModel


type ViewMode
    = Admin
    | Present


type alias UserModel =
    { question : Questions.Question
    , comment : String
    , commentSubmitStatus : SubmitStatus
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
    , currentQuestion : Questions.CurrentQuestion
    , adminSessions : Set SessionId
    , comments : List Comment
    , bannedUsers : Set SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedHowAreYou Questions.Happiness
    | PressedHowExperiencedAreYouWithElm Questions.ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming Questions.ExperienceLevel
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
    = ChoseHowAreYou Questions.Happiness
    | ChoseHowExperiencedAreYouWithElm Questions.ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming Questions.ExperienceLevel
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
    { howAreYou : List Questions.Happiness
    , howExperiencedAreYouWithElm : List Questions.ExperienceLevel
    , howExperiencedAreYouWithProgramming : List Questions.ExperienceLevel
    , whatCountryAreYouFrom : List Country
    , attributeQuestionAnswers : Dict SessionId Questions.AttributeQuestionAnswers
    , comments : List Comment
    }


type ToFrontend
    = SetAdminMode Questions.CurrentQuestion AdminData
    | StreamAttributeQuestionAnswer SessionId Questions.AttributeQuestionAnswer
    | StreamComment Comment
    | UpdateAdmin AdminData
    | SetCurrentQuestion Questions.CurrentQuestion
    | PostCommentResponse
    | RemoveAllBansResponse (List Comment)


type alias Comment =
    { sessionId : SessionId
    , text : NonemptyString
    , time : Time.Posix
    }
