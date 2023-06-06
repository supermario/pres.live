module Evergreen.V4.Questions exposing (..)

import Countries


type Happiness
    = Good
    | NotGood


type ExperienceLevel
    = Expert
    | Intermediate
    | Beginner


type AttendanceReason
    = Social
    | Business
    | BreakFromWork
    | CuratedDiscovery
    | YoutubeIsBetterIRL
    | Love
    | AttendanceOther


type Profession
    = Developer
    | Academic
    | Scientist
    | Student
    | Management
    | ProfessionOther


type Experience
    = OneToTwoYears
    | TwoPlusYears
    | FivePlusYears
    | TenPlusYears


type Scale
    = TenMillionPlus
    | OneMillionPlus
    | HundredThousandPlus
    | TenThousandPlus
    | OneThousandPlus
    | ZeroToOneThousand


type Language
    = C
    | Clojure
    | CPlusPlus
    | CSharp
    | Elixir
    | Elm
    | Erlang
    | FSharp
    | Go
    | Haskell
    | Idris
    | Java
    | Javascript
    | Kotlin
    | OCaml
    | PHP
    | Python
    | ReasonML
    | Ruby
    | Rust
    | Scala
    | Swift
    | Typescript
    | Unison


type AttributeQuestionAnswer
    = AttendanceReasonAnswer (Maybe (List AttendanceReason))
    | ProfessionAnswer (Maybe (List Profession))
    | ExperienceAnswer (Maybe Experience)
    | ScaleAnswer (Maybe Scale)
    | LanguagesAnswer (Maybe (List Language))


type alias QuestionOption =
    { comment : Bool
    , emoji : String
    , text : String
    }


type alias NormalisedQuestion =
    { multiselect : Bool
    , hideLabelInButton : Bool
    , sortResultsByCount : Bool
    , title : String
    , options : List QuestionOption
    }


type Question
    = HowAreYou (Maybe Happiness)
    | HowExperiencedAreYouWithElm (Maybe ExperienceLevel)
    | HowExperiencedAreYouWithProgramming (Maybe ExperienceLevel)
    | WhatCountryAreYouFrom (Maybe Countries.Country)
    | AttributeQuestion AttributeQuestionAnswer
    | NormalisedQuestionA NormalisedQuestion (List String)


type AttributeType
    = AttendanceReason
    | Profession
    | Experience
    | Scale
    | Languages


type CurrentQuestion
    = IntroScreen
    | HowAreYou_
    | HowExperiencedAreYouWithElm_
    | HowExperiencedAreYouWithProgramming_
    | WhatCountryAreYouFrom_
    | AttributeQuestion_ AttributeType
    | NormalisedQuestion_ NormalisedQuestion


type alias AttributeQuestionAnswers =
    { attendanceReasons : Maybe (List AttendanceReason)
    , professions : Maybe (List Profession)
    , experience : Maybe Experience
    , scale : Maybe Scale
    , languages : Maybe (List Language)
    }
