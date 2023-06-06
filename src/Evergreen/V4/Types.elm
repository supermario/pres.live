module Evergreen.V4.Types exposing (..)

import Browser
import Countries
import Dict
import Evergreen.V4.Questions
import Lamdera
import Set
import String.Nonempty
import Time
import Url


type SubmitStatus
    = NotSubmitted
    | Submitting


type alias UserModel =
    { question : Maybe Evergreen.V4.Questions.Question
    , comment : String
    , commentSubmitStatus : SubmitStatus
    , userCount : Int
    }


type ViewMode
    = Admin
    | Present


type alias Comment =
    { sessionId : Lamdera.SessionId
    , text : String.Nonempty.NonemptyString
    , time : Time.Posix
    }


type alias AdminData =
    { howAreYou : List Evergreen.V4.Questions.Happiness
    , howExperiencedAreYouWithElm : List Evergreen.V4.Questions.ExperienceLevel
    , howExperiencedAreYouWithProgramming : List Evergreen.V4.Questions.ExperienceLevel
    , whatCountryAreYouFrom : List Countries.Country
    , attributeQuestionAnswers : Dict.Dict Lamdera.SessionId Evergreen.V4.Questions.AttributeQuestionAnswers
    , normalizedQuestionAnswers : Dict.Dict Lamdera.SessionId (Dict.Dict String (List String))
    , comments : List Comment
    }


type FrontendModel
    = IsAdmin UserModel ViewMode Evergreen.V4.Questions.CurrentQuestion AdminData
    | IsUser UserModel


type alias BackendModel =
    { howAreYou : Dict.Dict Lamdera.SessionId Evergreen.V4.Questions.Happiness
    , howExperiencedAreYouWithElm : Dict.Dict Lamdera.SessionId Evergreen.V4.Questions.ExperienceLevel
    , howExperiencedAreYouWithProgramming : Dict.Dict Lamdera.SessionId Evergreen.V4.Questions.ExperienceLevel
    , whatCountryAreYouFrom : Dict.Dict Lamdera.SessionId Countries.Country
    , attributeQuestionAnswers : Dict.Dict Lamdera.SessionId Evergreen.V4.Questions.AttributeQuestionAnswers
    , normalizedQuestionAnswers : Dict.Dict Lamdera.SessionId (Dict.Dict String (List String))
    , currentQuestion : Evergreen.V4.Questions.CurrentQuestion
    , adminSessions : Set.Set Lamdera.SessionId
    , comments : List Comment
    , bannedUsers : Set.Set Lamdera.SessionId
    , sessions : Set.Set ( Lamdera.SessionId, Lamdera.ClientId )
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedHowAreYou Evergreen.V4.Questions.Happiness
    | PressedHowExperiencedAreYouWithElm Evergreen.V4.Questions.ExperienceLevel
    | PressedHowExperiencedAreYouWithProgramming Evergreen.V4.Questions.ExperienceLevel
    | PressedWhatCountryAreYouFrom Countries.Country
    | PressedAttributeQuestionAnswer Evergreen.V4.Questions.AttributeQuestionAnswer
    | PressedNormalisedQuestionAnswer String (List String)
    | AdminPressedNextQuestion
    | AdminPressedPreviousQuestion
    | AdminPressedReset
    | AdminToggledMode
    | TypedComment String
    | PressedSubmitComment
    | PressedBanUser Lamdera.SessionId
    | PressedRemoveAllBans
    | Noop String


type ToBackend
    = ChoseHowAreYou Evergreen.V4.Questions.Happiness
    | ChoseHowExperiencedAreYouWithElm Evergreen.V4.Questions.ExperienceLevel
    | ChoseHowExperiencedAreYouWithProgramming Evergreen.V4.Questions.ExperienceLevel
    | ChoseWhatCountryAreYouFrom Countries.Country
    | PressedAttributeQuestionAnswer_ Evergreen.V4.Questions.AttributeQuestionAnswer
    | PressedNormalisedQuestionAnswer_ String (List String)
    | AdminAuth String
    | AdminRequestNextQuestion
    | AdminPressedPreviousQuestion_
    | AdminRequestReset
    | PostCommentRequest String.Nonempty.NonemptyString
    | BanUserRequest Lamdera.SessionId
    | RemoveAllBansRequest


type BackendMsg
    = UserConnected Lamdera.SessionId Lamdera.ClientId
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix


type ToFrontend
    = SetAdminMode Evergreen.V4.Questions.CurrentQuestion AdminData
    | StreamAttributeQuestionAnswer Lamdera.SessionId Evergreen.V4.Questions.AttributeQuestionAnswer
    | StreamNormalizedQuestionAnswer Lamdera.SessionId String (List String)
    | StreamComment Comment
    | UpdateAdmin AdminData
    | SetCurrentQuestion Evergreen.V4.Questions.CurrentQuestion
    | PostCommentResponse
    | RemoveAllBansResponse (List Comment)
    | UserCountChanged Int
