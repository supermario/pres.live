module Questions exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import Lamdera
import List.Extra as List


all =
    []
        ++ [ IntroScreen
           , AttributeQuestion_ AttendanceReason
           , AttributeQuestion_ Profession
           , AttributeQuestion_ Experience
           , AttributeQuestion_ Scale
           , AttributeQuestion_ Languages
           ]
        ++ (openQuestions |> List.map NormalisedQuestion_)
        ++ [ HowAreYou_
           , HowExperiencedAreYouWithElm_
           , HowExperiencedAreYouWithProgramming_
           , WhatCountryAreYouFrom_
           ]


attributeQuestion : AttributeType -> NormalisedQuestion
attributeQuestion attributeType =
    case attributeType of
        AttendanceReason ->
            { title = "Why are you here?"
            , multiselect = True
            , options =
                allAttendanceReasons |> List.map attendanceReasonToOption
            }

        Profession ->
            { title = "What field(s) are you in?"
            , multiselect = True
            , options =
                allProfessions |> List.map professionToOption
            }

        Experience ->
            { title = "Experience in your field(s)?"
            , multiselect = False
            , options =
                allExperiences |> List.map experienceToOption
            }

        Scale ->
            { title = "What is the scale of your project's (concurrent users)?"
            , multiselect = False
            , options =
                allScales |> List.map scaleToOption
            }

        Languages ->
            { title = "Languages you work with currently"
            , multiselect = True
            , options =
                allLanguages |> List.map languageToOption
            }


openQuestions : List NormalisedQuestion
openQuestions =
    [ basicQuestion "Is the added complexity is worth it?"
    , basicQuestion "The systems I work on are getting more complex."
    , basicQuestion "I feel well equipped to deal with or reduce complexity."
    , basicQuestion "Is this a boundary?"
    , { title = "Someone on our backend team changed a field name in the API.\nWe’ll find out at:"
      , multiselect = False
      , options =
            [ { comment = True, emoji = "💻", text = "Dev time" }
            , { comment = True, emoji = "🤖", text = "Build/Test/CI time" }
            , { comment = True, emoji = "🔥", text = "Run time" }
            ]
      }
    , { title = "A 3rd party changed a field name in their API. \nWe’ll find out at:"
      , multiselect = False
      , options =
            [ { comment = True, emoji = "💻", text = "Dev time" }
            , { comment = True, emoji = "🤖", text = "Build/Test/CI time" }
            , { comment = True, emoji = "🔥", text = "Run time" }
            ]
      }
    , basicQuestion "Does your code handle HTTP 418 error codes?"
    , { title = "What percentage of your code is glue code?"
      , multiselect = False
      , options =
            [ { comment = True, emoji = "🟢", text = "<10%" }
            , { comment = True, emoji = "🟡", text = "<20%" }
            , { comment = True, emoji = "🟠", text = "<40%" }
            , { comment = True, emoji = "🔴", text = "<80%" }
            , { comment = True, emoji = "🔥", text = "Like, all of it" }
            ]
      }
    , { title = "How was it for you?"
      , multiselect = False
      , options =
            [ { comment = True, emoji = "🤩", text = "Great" }
            , { comment = True, emoji = "🤷🏼\u{200D}♀️", text = "Alright" }
            , { comment = True, emoji = "🙅🏿", text = "Bad" }
            ]
      }
    ]



-- @TODO rename this to QuestionAnswer


type Question
    = HowAreYou (Maybe Happiness)
    | HowExperiencedAreYouWithElm (Maybe ExperienceLevel)
    | HowExperiencedAreYouWithProgramming (Maybe ExperienceLevel)
    | WhatCountryAreYouFrom (Maybe Country)
    | AttributeQuestion AttributeQuestionAnswer
    | NormalisedQuestionA NormalisedQuestion (List String)


type CurrentQuestion
    = IntroScreen
    | HowAreYou_
    | HowExperiencedAreYouWithElm_
    | HowExperiencedAreYouWithProgramming_
    | WhatCountryAreYouFrom_
    | AttributeQuestion_ AttributeType
    | NormalisedQuestion_ NormalisedQuestion


type Happiness
    = Good
    | NotGood


type ExperienceLevel
    = Expert
    | Intermediate
    | Beginner


basicQuestion : String -> NormalisedQuestion
basicQuestion title =
    { title = title
    , multiselect = False
    , options = standardYesNoMaybeOptions
    }


standardYesNoMaybeOptions : List QuestionOption
standardYesNoMaybeOptions =
    [ { comment = False, emoji = "✅", text = "Yes" }
    , { comment = False, emoji = "🤔", text = "Maybe?" }
    , { comment = False, emoji = "🙅🏼\u{200D}♀️", text = "No" }
    ]


type alias QuestionOption =
    { comment : Bool, emoji : String, text : String }


type alias NormalisedQuestion =
    { multiselect : Bool, title : String, options : List QuestionOption }


type alias NormalisedQuestionAnswer =
    List String


type alias AttributeQuestionAnswers =
    { attendanceReasons : Maybe (List AttendanceReason)
    , professions : Maybe (List Profession)
    , experience : Maybe Experience
    , scale : Maybe Scale
    , languages : Maybe (List Language)
    }


attributeQuestionAnswersForAttributeType : AttributeType -> Dict Lamdera.SessionId AttributeQuestionAnswers -> List String
attributeQuestionAnswersForAttributeType attributeType attributeQuestionAnswersDict =
    let
        attributeQuestionAnswers =
            attributeQuestionAnswersDict |> Dict.values
    in
    case attributeType of
        AttendanceReason ->
            attributeQuestionAnswers
                |> List.map (.attendanceReasons >> Maybe.map (List.map attendanceReasonToString) >> Maybe.withDefault [])
                |> List.concat

        Profession ->
            attributeQuestionAnswers
                |> List.map (.professions >> Maybe.map (List.map professionToString) >> Maybe.withDefault [])
                |> List.concat

        Experience ->
            attributeQuestionAnswers
                |> List.filterMap (.experience >> Maybe.map experienceToString)

        Scale ->
            attributeQuestionAnswers
                |> List.filterMap (.scale >> Maybe.map scaleToString)

        Languages ->
            attributeQuestionAnswers
                |> List.map (.languages >> Maybe.map (List.map languageToString) >> Maybe.withDefault [])
                |> List.concat


emptyAttributeAnswers : AttributeQuestionAnswers
emptyAttributeAnswers =
    { attendanceReasons = Nothing
    , professions = Nothing
    , experience = Nothing
    , scale = Nothing
    , languages = Nothing
    }


updateAnswers : AttributeQuestionAnswer -> AttributeQuestionAnswers -> AttributeQuestionAnswers
updateAnswers attributeQuestionAnswer answers =
    case attributeQuestionAnswer of
        AttendanceReasonAnswer maybeAttendanceReasons ->
            { answers | attendanceReasons = maybeAttendanceReasons }

        ProfessionAnswer maybeProfessions ->
            { answers | professions = maybeProfessions }

        ExperienceAnswer maybeExperience ->
            { answers | experience = maybeExperience }

        ScaleAnswer maybeScale ->
            { answers | scale = maybeScale }

        LanguagesAnswer maybeLanguages ->
            { answers | languages = maybeLanguages }


type AttributeQuestionAnswer
    = AttendanceReasonAnswer (Maybe (List AttendanceReason))
    | ProfessionAnswer (Maybe (List Profession))
    | ExperienceAnswer (Maybe Experience)
    | ScaleAnswer (Maybe Scale)
    | LanguagesAnswer (Maybe (List Language))


attributeTypeToAttributeQuestionAnswerDefault attributeType =
    case attributeType of
        AttendanceReason ->
            AttendanceReasonAnswer Nothing

        Profession ->
            ProfessionAnswer Nothing

        Experience ->
            ExperienceAnswer Nothing

        Scale ->
            ScaleAnswer Nothing

        Languages ->
            LanguagesAnswer Nothing


optionTextToAttributeQuestionAnswer : AttributeQuestionAnswer -> String -> Maybe AttributeQuestionAnswer
optionTextToAttributeQuestionAnswer attributeQuestionAnswer optionText =
    case attributeQuestionAnswer of
        AttendanceReasonAnswer maybeAttendanceReasons ->
            handleList AttendanceReasonAnswer maybeAttendanceReasons allAttendanceReasons attendanceReasonToString optionText

        ProfessionAnswer maybeProfessions ->
            handleList ProfessionAnswer maybeProfessions allProfessions professionToString optionText

        ExperienceAnswer maybeExperience ->
            handleSingular ExperienceAnswer maybeExperience allExperiences experienceToString optionText

        ScaleAnswer maybeScale ->
            handleSingular ScaleAnswer maybeScale allScales scaleToString optionText

        LanguagesAnswer maybeLanguages ->
            handleList LanguagesAnswer maybeLanguages allLanguages languageToString optionText


handleList :
    (Maybe (List a) -> AttributeQuestionAnswer)
    -> Maybe (List a)
    -> List a
    -> (a -> String)
    -> String
    -> Maybe AttributeQuestionAnswer
handleList wrapper existingAnswerM allAnswers toString optionText =
    allAnswers
        |> List.filter (toString >> (==) optionText)
        |> List.head
        |> Maybe.map
            (\matchingAnswer ->
                existingAnswerM
                    |> Maybe.map (listToggleValue matchingAnswer)
                    |> Maybe.withDefault [ matchingAnswer ]
                    |> Just
                    |> wrapper
            )


listToggleValue : a -> List a -> List a
listToggleValue value list =
    if List.member value list then
        List.filter ((/=) value) list

    else
        value :: list


handleSingular :
    (Maybe a -> AttributeQuestionAnswer)
    -> Maybe a
    -> List a
    -> (a -> String)
    -> String
    -> Maybe AttributeQuestionAnswer
handleSingular wrapper answerM allAnswers toString optionText =
    allAnswers
        |> List.filter (toString >> (==) optionText)
        |> List.head
        |> Maybe.map (Just >> wrapper)


type AttributeType
    = AttendanceReason
    | Profession
    | Experience
    | Scale
    | Languages


allAttributeTypes : List AttributeType
allAttributeTypes =
    [ AttendanceReason, Profession, Experience, Scale, Languages ]


type AttendanceReason
    = Social
    | Business
    | BreakFromWork
    | CuratedDiscovery
    | YoutubeIsBetterIRL
    | Love
    | AttendanceOther


allAttendanceReasons : List AttendanceReason
allAttendanceReasons =
    [ Social, Business, BreakFromWork, CuratedDiscovery, YoutubeIsBetterIRL, Love, AttendanceOther ]


attendanceReasonToString attendanceReason =
    case attendanceReasonToOption attendanceReason of
        { comment, emoji, text } ->
            text


attendanceReasonToOption : AttendanceReason -> QuestionOption
attendanceReasonToOption attendanceReason =
    case attendanceReason of
        Social ->
            { comment = False, emoji = "🤝", text = "Social" }

        Business ->
            { comment = False, emoji = "💼", text = "Business" }

        BreakFromWork ->
            { comment = False, emoji = "🌴", text = "Break from work" }

        CuratedDiscovery ->
            { comment = False, emoji = "🔭", text = "Curated Discovery" }

        YoutubeIsBetterIRL ->
            { comment = False, emoji = "📺", text = "Youtube is better IRL" }

        Love ->
            { comment = False, emoji = "💓", text = "Love" }

        AttendanceOther ->
            { comment = True, emoji = "❓", text = "Other" }


type Profession
    = Developer
    | Academic
    | Scientist
    | Student
    | Management
    | ProfessionOther


allProfessions : List Profession
allProfessions =
    [ Developer, Academic, Scientist, Student, ProfessionOther ]


professionToString : Profession -> String
professionToString profession =
    case professionToOption profession of
        { comment, emoji, text } ->
            text


professionToOption : Profession -> QuestionOption
professionToOption profession =
    case profession of
        Developer ->
            { comment = False, emoji = "👩🏼\u{200D}💻", text = "Developer" }

        Academic ->
            { comment = False, emoji = "🧙🏼\u{200D}♀️", text = "Academic" }

        Scientist ->
            { comment = False, emoji = "🧑🏻\u{200D}🔬", text = "Scientist" }

        Student ->
            { comment = False, emoji = "🧑🏾\u{200D}🎓", text = "Student" }

        Management ->
            { comment = False, emoji = "", text = "Management" }

        ProfessionOther ->
            { comment = True, emoji = "❓", text = "Other" }


type Experience
    = OneToTwoYears
    | TwoPlusYears
    | FivePlusYears
    | TenPlusYears


allExperiences : List Experience
allExperiences =
    [ OneToTwoYears, TwoPlusYears, FivePlusYears, TenPlusYears ]


experienceToString : Experience -> String
experienceToString experience =
    case experienceToOption experience of
        { comment, emoji, text } ->
            text


experienceToOption : Experience -> QuestionOption
experienceToOption experience =
    case experience of
        OneToTwoYears ->
            { comment = False, emoji = "🌰", text = "1-2 years" }

        TwoPlusYears ->
            { comment = False, emoji = "🌱", text = "2+ years" }

        FivePlusYears ->
            { comment = False, emoji = "\u{1FAB4}", text = "5+ years" }

        TenPlusYears ->
            { comment = False, emoji = "🌳", text = "10+ years" }


type Scale
    = TenMillionPlus
    | OneMillionPlus
    | HundredThousandPlus
    | TenThousandPlus
    | OneThousandPlus
    | ZeroToOneThousand


allScales : List Scale
allScales =
    [ TenMillionPlus, OneMillionPlus, HundredThousandPlus, TenThousandPlus, OneThousandPlus, ZeroToOneThousand ]


scaleToString : Scale -> String
scaleToString scale =
    case scaleToOption scale of
        { comment, emoji, text } ->
            text


scaleToOption : Scale -> QuestionOption
scaleToOption scale =
    case scale of
        TenMillionPlus ->
            { comment = False, emoji = "", text = "10M+" }

        OneMillionPlus ->
            { comment = False, emoji = "", text = "1M+" }

        HundredThousandPlus ->
            { comment = False, emoji = "", text = "100K+" }

        TenThousandPlus ->
            { comment = False, emoji = "", text = "10K+" }

        OneThousandPlus ->
            { comment = False, emoji = "", text = "1K+" }

        ZeroToOneThousand ->
            { comment = False, emoji = "", text = "0-1K" }


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


allLanguages : List Language
allLanguages =
    [ C, Clojure, CPlusPlus, CSharp, Elixir, Elm, Erlang, FSharp, Go, Haskell, Idris, Java, Javascript, Kotlin, OCaml, Python, ReasonML, Ruby, Rust, Scala, Swift, Typescript, Unison ]


languageToString : Language -> String
languageToString language =
    case languageToOption language of
        { comment, emoji, text } ->
            text


languageToOption : Language -> QuestionOption
languageToOption language =
    case language of
        C ->
            { comment = False, emoji = "", text = "C" }

        Clojure ->
            { comment = False, emoji = "", text = "Clojure" }

        CPlusPlus ->
            { comment = False, emoji = "", text = "C++" }

        CSharp ->
            { comment = False, emoji = "", text = "C#" }

        Elixir ->
            { comment = False, emoji = "", text = "Elixir" }

        Elm ->
            { comment = False, emoji = "", text = "Elm" }

        Erlang ->
            { comment = False, emoji = "", text = "Erlang" }

        FSharp ->
            { comment = False, emoji = "", text = "F#" }

        Go ->
            { comment = False, emoji = "", text = "Go" }

        Haskell ->
            { comment = False, emoji = "", text = "Haskell" }

        Idris ->
            { comment = False, emoji = "", text = "Idris" }

        Java ->
            { comment = False, emoji = "", text = "Java" }

        Javascript ->
            { comment = False, emoji = "", text = "Javascript" }

        Kotlin ->
            { comment = False, emoji = "", text = "Kotlin" }

        OCaml ->
            { comment = False, emoji = "", text = "OCaml" }

        PHP ->
            { comment = False, emoji = "", text = "PHP" }

        Python ->
            { comment = False, emoji = "", text = "Python" }

        ReasonML ->
            { comment = False, emoji = "", text = "ReasonML" }

        Ruby ->
            { comment = False, emoji = "", text = "Ruby" }

        Rust ->
            { comment = False, emoji = "", text = "Rust" }

        Scala ->
            { comment = False, emoji = "", text = "Scala" }

        Swift ->
            { comment = False, emoji = "", text = "Swift" }

        Typescript ->
            { comment = False, emoji = "", text = "Typescript" }

        Unison ->
            { comment = False, emoji = "", text = "Unison" }



-- type Question
--     = TextQuestion
--         { question : String
--         , options : List String
--         , votes : Dict.Dict Lamdera.SessionId String
--         }
-- all : Dict String Question
-- all =
--     Dict.fromList
--         [ ( "How are you feeling?", TextQuestion { question = "How are you feeling?", options = [ "👍", "👎" ], votes = Dict.empty } )
--         , ( "Your programming level", TextQuestion { question = "Your programming level", options = [ "Beginner", "Confident", "Professional" ], votes = Dict.empty } )
--         , ( "Your programming level in ELM", TextQuestion { question = "Your programming level in ELM", options = [ "Beginner", "Confident", "Professional" ], votes = Dict.empty } )
--         , ( "What are you using elm for?", TextQuestion { question = "What are you using elm for?", options = [ "Hobby", "Work", "Microwave" ], votes = Dict.empty } )
--         , ( "Where are you from?", TextQuestion { question = "Where are you from?", options = [ "Earth", "Deep Space" ], votes = Dict.empty } )
--         -- , ( "What country are you from?", CountryQuestion { votes = Dict.empty } )
--         ]
