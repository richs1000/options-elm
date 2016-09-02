module Question exposing (..)

import Options exposing (..)
import RandomStuff exposing (pickOne, pickABunch, compressList)
import Debug exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    -- use valOf to extract
    if index == 1 then
        let
            rOption =
                randomSomething randomValues

            question' =
                [ "What is the value of ans after the following ML expressions are evaluated?"
                , ""
                , "val e = " ++ (optionToString rOption)
                , "val ans = valOf e"
                , ""
                ]

            answer' =
                ( stringValOf rOption
                , "Correct."
                )

            distractors' =
                [ ( "None"
                  , "Incorrect."
                  )
                , ( optionToString rOption
                  , "Incorrect."
                  )
                ]
        in
            { question = question'
            , distractors = distractors'
            , answer = answer'
            , format = MultipleChoice
            }
        -- isSome
    else if index == 2 then
        let
            rOption =
                randomOption randomValues

            question' =
                [ "What is the value of ans after the following ML expressions are evaluated?"
                , ""
                , "val e = " ++ (optionToString rOption)
                , "val ans = isSome e"
                , ""
                ]

            answer' =
                ( myIsSomeString rOption
                , "Correct."
                )

            distractors' =
                [ ( "None"
                  , "Incorrect. isSome returns true if an option has a SOME value and false if an option has a value of NONE"
                  )
                , ( toString (not (myIsSome rOption))
                  , "Incorrect. isSome returns true if an option has a SOME value and false if an option has a value of NONE"
                  )
                ]
        in
            { question = question'
            , distractors = distractors'
            , answer = answer'
            , format = MultipleChoice
            }
        -- types
    else
        let
            rOption =
                randomOption randomValues

            question' =
                [ "What is the type of e?"
                , ""
                , "val e = " ++ (optionToString rOption)
                , ""
                ]

            -- answer' =
            --     tupOfTups
            --         |> extractTupleFromTuple interiorIndex
            --         |> extractItemFromTuple exteriorIndex
            --         |> tupleItemToString
            --
            -- distractors' =
            --     Debug.log "compressed "
            --         (compressList
            --             (tupleOfTuplesToFlatListOfStrings tupOfTups)
            --         )
            --
            -- ( _, distractors'' ) =
            --     List.partition (\d -> d == answer') distractors'
            answer' =
                (optionToTypeString rOption "option")

            distractors' =
                [ "NONE"
                , "SOME int"
                , "SOME bool"
                , "SOME string"
                , "int option"
                , "bool option"
                , "string option"
                , "'a option"
                ]

            ( _, distractors'' ) =
                List.partition (\d -> d == answer') distractors'
        in
            { question = question'
            , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors''
            , answer = ( answer', "Correct" )
            , format = MultipleChoice
            }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
