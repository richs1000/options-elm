module Options exposing (..)

import RandomStuff exposing (..)


type RandomOption
    = RandomInt Int
    | RandomString String
    | RandomBool Bool
    | RandomNone


type RandomOptionTypes
    = RInt
    | RString
    | RBool
    | RNone


optionToTypeString : RandomOption -> String -> String
optionToTypeString ro suffix =
    case ro of
        RandomInt _ ->
            "int " ++ suffix

        RandomString _ ->
            "string " ++ suffix

        RandomBool _ ->
            "bool " ++ suffix

        RandomNone ->
            "'a " ++ suffix


optionToString : RandomOption -> String
optionToString ro =
    case ro of
        RandomInt i ->
            "SOME " ++ toString i

        RandomString s ->
            "SOME " ++ toString s

        RandomBool b ->
            "SOME " ++ toString b

        RandomNone ->
            "NONE"


randomOptionHelper : List Int -> List RandomOptionTypes -> RandomOption
randomOptionHelper randomValues optionTypes =
    let
        randStrings =
            [ "dog", "cat", "pig", "moose", "cow", "bird" ]

        rType =
            pickOne randomValues optionTypes RInt
    in
        case rType of
            RBool ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) [ 1, 2 ] 1
                in
                    if rVal == 1 then
                        RandomBool True
                    else
                        RandomBool False

            RInt ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) [0..9] 1
                in
                    RandomInt rVal

            RString ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) randStrings "cat"
                in
                    RandomString rVal

            RNone ->
                RandomNone


randomSomething : List Int -> RandomOption
randomSomething randomValues =
    randomOptionHelper randomValues [ RInt, RString, RBool ]


randomOption : List Int -> RandomOption
randomOption randomValues =
    randomOptionHelper randomValues [ RInt, RString, RBool, RNone ]


stringValOf : RandomOption -> String
stringValOf ro =
    case ro of
        RandomInt i ->
            toString i

        RandomString s ->
            toString s

        RandomBool b ->
            toString b

        RandomNone ->
            "NONE"


myIsSome : RandomOption -> Bool
myIsSome ro =
    case ro of
        RandomNone ->
            False

        _ ->
            True


myIsSomeString : RandomOption -> String
myIsSomeString ro =
    toString (myIsSome ro)
