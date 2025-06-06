----------------------------------------------------------------------
--
-- DynamoDB/AppState.elm
-- Help for storing application state in DynanoDB.
-- Copyright (c) 2022-2025 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------
--
-- See TODO below.
-- Doesn't handle more than 98 updates.
-- Doesn't handle change in DB with stores pending.
--
----------------------------------------------------------------------


module DynamoDB.AppState exposing
    ( AppState, makeAppState
    , Error, accountIncomplete, save, idle, isIdleTime
    , Updates, update, initialLoad
    , isActive, goActive
    , renderAccount
    , store, mergeAccount
    )

{-| Support for storing application state in DynamoDB.

Call `initialLoad` when your application starts, to get the `saveCount` and
`keyCounts` from DynamoDB, and compare them to local versions, usually
saved in `LocalStorage`.

Specify key/value pairs to save by calling `save`.

Once a second, call `idle` to make sure unstored changes get pushed to DynamoDB, at `idlePeriod` intervals.

Call `update` to pull changes from DynamoDB, at `updatePeriod` intervals.

Call `isActive` at `updateActive` intervals to make your app stop calling `update`, until the user does something to make it necessary.

Call `goActive` when the user does something that requires pulling changes from other browsers.


# State

@docs AppState, makeAppState


# Updating state

@docs Error, accountIncomplete, save, idle, isIdleTime


# Getting updates from other browsers.

@docs Updates, update, initialLoad


# Stop calling `update` periodically.

@docs isActive, goActive


# User Interface

@docs renderAccount


# Internals

@docs store, mergeAccount

-}

import Dict exposing (Dict)
import DynamoDB exposing (TransactGetItem, TransactGetItemValue, TransactWrite(..))
import DynamoDB.Types as Types
    exposing
        ( Account
        , AttributeValue(..)
        , Item
        , Key(..)
        , TableName
        )
import Html exposing (Html, input, label, p, span, text)
import Html.Attributes exposing (checked, size, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Task exposing (Task)


{-| Track updates for DynamoDB persistence.
-}
type alias AppState =
    { account : Account
    , idlePeriod : Int
    , updatePeriod : Int
    , activePeriod : Int
    , keyName : String
    , valueAttributeName : String
    , saveCountAttributeName : String
    , saveCountKey : String
    , keyCountsKey : String
    , keyPrefix : Maybe String

    -- Above here is usually constant. Below changes.
    , lastIdleTime : Int
    , lastUpdateTime : Int
    , saveCount : Int
    , lastActiveTime : Int
    , updates : Dict String (Maybe Value)
    , keyCounts : Dict String Int
    }


{-| Make an AppState with good defaults.
-}
makeAppState : Account -> AppState
makeAppState account =
    { account = account
    , idlePeriod = 2000
    , updatePeriod = 10000
    , activePeriod = 5000
    , keyName = "key"
    , valueAttributeName = "value"
    , saveCountAttributeName = "saveCount"
    , saveCountKey = "DynamoDB.saveCount"
    , keyCountsKey = "DynamoDB.keyCounts"
    , keyPrefix = Nothing

    -- Times are in milliseconds, as returned by `Time.posixToMillis` applied
    -- to the result of `Time.now`.
    , lastIdleTime = 0
    , lastUpdateTime = 0
    , lastActiveTime = -1 --active by deafult
    , saveCount = 0
    , updates = Dict.empty
    , keyCounts = Dict.empty
    }


{-| DynamoDB allows a maximum of 100 `TransactItems`.

We use two for the update count and the key -> count map.

-}
maxSaves : Int
maxSaves =
    100 - 2


{-| The `Error` in the result from executing the task returned by `save` and `idle`.
-}
type alias Error =
    { appState : AppState
    , error : Types.Error
    }


{-| Return `True` if an `Account` does not have all its field filled in.

Do NOT call `save`, `idle`, or `update` if this returns `True`.
You'll get an error on attempting to connect to the DynamoDB server.

-}
accountIncomplete : AppState -> Bool
accountIncomplete appState =
    let
        account =
            appState.account
    in
    (account.accessKey == "")
        || (account.secretKey == "")
        || (account.tableName == "")


{-| Add a new key/value pair to the DynamoDB store.

Don't actually push it to DynamoDB you've accumulated as many keys as
can fit in one transaction. Call `idle` to force a push.

The task that saves to DynamoDB has as its value the number of keys
that were saved. It does nothing and has a value of 0, if the
save doesn't yet fill a transaction.

-}
save : Int -> String -> Maybe Value -> AppState -> Maybe ( AppState, Task Error Int )
save time key value appState =
    let
        state =
            { appState
                | updates = Dict.insert key value appState.updates
                , lastIdleTime = time
            }
    in
    if Dict.size state.updates < maxSaves then
        Just
            ( state
            , Task.succeed 0
            )

    else
        store time state


{-| Is it time to query the database for updates from other apps?
-}
isIdleTime : Int -> AppState -> Bool
isIdleTime time appState =
    time > appState.lastIdleTime + appState.idlePeriod


{-| Push the saved changes to DynamoDB if the idle time has elapsed.
-}
idle : Int -> AppState -> Maybe ( AppState, Task Error Int )
idle time appState =
    if not <| isIdleTime time appState then
        Nothing

    else
        store time appState


{-| The successful Result of `update` and `initialLoad`.
-}
type alias Updates =
    { saveCount : Int
    , keyCounts : Dict String Int
    , updates : Dict String (Maybe String)
    }


{-| Probe the database for updates from other browsers.

Returns `Nothing` if it's not yet time for the update.

The Task's value will be `Nothing` if your state is up-to-date with
the database. Otherwise, you'll need to update your copy of the
`AppState` with the returned `updates.saveCount` & `updates.keyCounts`,
and process the `updates.updates`, likely running appropriate JSON
decoders on the values (which will be `Nothing` if the key was deleted
in the database).

-}
update : Int -> AppState -> Maybe ( AppState, Task Error (Maybe Updates) )
update time appState =
    if not <| isIdleTime time appState then
        Nothing

    else
        Just <|
            ( { appState | lastUpdateTime = time }
            , initialLoad appState appState.saveCount appState.keyCounts
            )


{-| Check for activity. Return True if active.
-}
isActive : Int -> AppState -> Bool
isActive time appState =
    (appState.lastActiveTime == -1)
        || (time <= appState.lastActiveTime + appState.activePeriod)


{-| Go active, usually due to UI action that requires update from afar.
-}
goActive : Int -> AppState -> AppState
goActive time appState =
    { appState | lastActiveTime = time }


makeKey : AppState -> String -> Key
makeKey appState key =
    let
        prefixedKey =
            case appState.keyPrefix of
                Nothing ->
                    key

                Just prefix ->
                    prefix ++ "." ++ key
    in
    SimpleKey ( appState.keyName, StringValue prefixedKey )


makeValueItem : AppState -> Value -> Item
makeValueItem appState value =
    DynamoDB.makeItem
        [ ( appState.valueAttributeName
          , StringValue <| JE.encode 0 value
          )
        , ( appState.saveCountAttributeName
          , StringValue <| String.fromInt appState.saveCount
          )
        ]


encodeKeyCounts : Dict String Int -> Value
encodeKeyCounts keyCounts =
    JE.dict identity JE.int keyCounts


{-| Push all saved updates to DynamoDB.
-}
store : Int -> AppState -> Maybe ( AppState, Task Error Int )
store time aState =
    if aState.updates == Dict.empty then
        Nothing

    else
        let
            saveCount =
                aState.saveCount + 1

            appState =
                -- `saveCount` is used by `makeValueItem`
                { aState | saveCount = saveCount }

            keyCounts =
                Dict.foldl
                    (\k v counts ->
                        Dict.insert k saveCount counts
                    )
                    appState.keyCounts
                    appState.updates

            tableName =
                appState.account.tableName

            saveWrites =
                List.map
                    (\( k, v ) ->
                        let
                            key =
                                makeKey appState k
                        in
                        case v of
                            Nothing ->
                                TransactWriteDelete
                                    { tableName = tableName
                                    , key = key
                                    }

                            Just val ->
                                TransactWritePut
                                    { tableName = tableName
                                    , key = Just key
                                    , item =
                                        makeValueItem appState val
                                    }
                    )
                <|
                    Dict.toList appState.updates

            keyCountsWrite =
                TransactWritePut
                    { tableName = tableName
                    , key = Just <| makeKey appState appState.keyCountsKey
                    , item = makeValueItem appState <| encodeKeyCounts keyCounts
                    }

            saveCountWrite =
                TransactWritePut
                    { tableName = tableName
                    , key = Just <| makeKey appState appState.saveCountKey
                    , item = makeValueItem appState <| JE.int saveCount
                    }

            state =
                { appState
                    | lastIdleTime = time
                    , lastUpdateTime = time
                    , saveCount = saveCount
                    , updates = Dict.empty
                    , keyCounts = keyCounts
                }
        in
        Just
            ( state
            , saveCountWrite
                :: (keyCountsWrite :: saveWrites)
                |> DynamoDB.transactWriteItems
                |> DynamoDB.send appState.account
                |> Task.andThen
                    (\() -> Task.succeed <| Dict.size appState.updates)
                |> Task.mapError
                    (\err ->
                        { appState = appState
                        , error = err
                        }
                    )
            )


makeGetItem : AppState -> String -> TransactGetItem
makeGetItem appState keyValue =
    { tableName = appState.account.tableName
    , key = makeKey appState keyValue
    , returnedAttributeNames = Just [ appState.valueAttributeName ]
    }


saveAndKeyCountsGets : AppState -> List TransactGetItem
saveAndKeyCountsGets appState =
    [ makeGetItem appState appState.saveCountKey
    , makeGetItem appState appState.keyCountsKey
    ]


{-| Load the `saveCount` and `keyCounts` from DynamoDB.

Also load any key/value pairs that are more recent in the database than
they are in the passed `keyCounts`:

    initialLoad appState saveCount keyCounts

-}
initialLoad : AppState -> Int -> Dict String Int -> Task Error (Maybe Updates)
initialLoad appState saveCount keyCounts =
    initialLoadInternal appState saveCount keyCounts
        |> Task.onError (Task.fail << Error appState)


initialLoadInternal : AppState -> Int -> Dict String Int -> Task Types.Error (Maybe Updates)
initialLoadInternal appState saveCount keyCounts =
    DynamoDB.transactGetItems (saveAndKeyCountsGets appState)
        |> DynamoDB.send appState.account
        |> Task.andThen (continueInitialLoad appState saveCount keyCounts)


getTransactGetItemValue : AppState -> String -> Maybe Item -> Decoder x -> Result Types.Error x
getTransactGetItemValue appState valueName maybeItem decoder =
    case getMaybeTransactGetItemValue appState valueName maybeItem decoder of
        Err err ->
            Err err

        Ok Nothing ->
            Err <| Types.DecodeError (valueName ++ ": no value")

        Ok (Just x) ->
            Ok x


getMaybeTransactGetItemValue : AppState -> String -> Maybe Item -> Decoder x -> Result Types.Error (Maybe x)
getMaybeTransactGetItemValue appState valueName maybeItem decoder =
    case maybeItem of
        Nothing ->
            Ok Nothing

        Just item ->
            case Dict.get appState.valueAttributeName item of
                Nothing ->
                    Err <| Types.DecodeError (valueName ++ ": missing value.")

                Just value ->
                    case value of
                        StringValue s ->
                            case JD.decodeString decoder s of
                                Err err ->
                                    Err <|
                                        Types.DecodeError
                                            (valueName
                                                ++ " "
                                                ++ JD.errorToString err
                                            )

                                Ok res ->
                                    Ok <| Just res

                        _ ->
                            Err <|
                                Types.DecodeError
                                    (valueName ++ ": not a StringValue")


unpackSaveCountAndKeyCountValues : AppState -> TransactGetItemValue -> TransactGetItemValue -> Result Types.Error ( Int, Dict String Int )
unpackSaveCountAndKeyCountValues appState saveCountValue keyCountValue =
    case getTransactGetItemValue appState appState.valueAttributeName saveCountValue.item JD.int of
        Err err ->
            Err err

        Ok saveCount ->
            case getTransactGetItemValue appState appState.valueAttributeName keyCountValue.item <| JD.dict JD.int of
                Err err ->
                    Err err

                Ok keyCounts ->
                    Ok ( saveCount, keyCounts )


continueInitialLoad : AppState -> Int -> Dict String Int -> List TransactGetItemValue -> Task Types.Error (Maybe Updates)
continueInitialLoad appState saveCount keyCounts values =
    case values of
        [ saveCountValue, keyCountValue ] ->
            case
                unpackSaveCountAndKeyCountValues appState
                    saveCountValue
                    keyCountValue
            of
                Err err ->
                    Task.fail err

                Ok ( savedCount, savedKeyCounts ) ->
                    doInitialLoadUpdates appState
                        saveCount
                        keyCounts
                        { saveCount = savedCount
                        , keyCounts = savedKeyCounts
                        , updates = Dict.empty
                        }

        _ ->
            Task.fail <|
                Types.DecodeError
                    "Other than two values from load of saveCount and keyCounts"


doInitialLoadUpdates : AppState -> Int -> Dict String Int -> Updates -> Task Types.Error (Maybe Updates)
doInitialLoadUpdates appState saveCount keyCounts res =
    let
        savedKeyCounts =
            res.keyCounts
    in
    if (saveCount > res.saveCount) || (keyCounts == savedKeyCounts) then
        Task.succeed Nothing

    else
        -- TODO: limit to 100 operations per transaction.
        -- For now, that will get an error.
        -- TODO: This needs to handle outstanding stores somehow.
        -- If `appState.updates` is not empty, we have a problem.
        -- Given the intended use, it shouldn't happen, but what if it does?
        let
            folder key count ( keys, gets ) =
                if Dict.get key keyCounts == Just count then
                    ( keys, gets )

                else
                    ( key :: keys, makeGetItem appState key :: gets )

            ( fetchKeys, fetchGets ) =
                Dict.foldr folder ( [], [] ) savedKeyCounts
        in
        DynamoDB.transactGetItems (saveAndKeyCountsGets appState ++ fetchGets)
            |> DynamoDB.send appState.account
            |> Task.andThen (continueInitialLoadUpdates appState res fetchKeys)


continueInitialLoadUpdates : AppState -> Updates -> List String -> List TransactGetItemValue -> Task Types.Error (Maybe Updates)
continueInitialLoadUpdates appState res keys values =
    case values of
        saveCountValue :: (keyCountValue :: otherValues) ->
            case
                unpackSaveCountAndKeyCountValues appState
                    saveCountValue
                    keyCountValue
            of
                Err err ->
                    Task.fail err

                Ok ( savedCount, savedKeyCounts ) ->
                    if
                        (savedCount /= res.saveCount)
                            || (savedKeyCounts /= res.keyCounts)
                    then
                        -- Somebody changed the saveCount or keyCounts
                        -- between our two transactions. Try again.
                        initialLoadInternal appState res.saveCount res.keyCounts

                    else
                        let
                            loop : List ( String, Maybe Item ) -> Dict String (Maybe String) -> Task Types.Error (Maybe Updates)
                            loop namesAndItems updates =
                                case namesAndItems of
                                    [] ->
                                        Task.succeed <|
                                            Just { res | updates = updates }

                                    ( key, item ) :: rest ->
                                        case
                                            getMaybeTransactGetItemValue
                                                appState
                                                key
                                                item
                                                JD.string
                                        of
                                            Err err ->
                                                Task.fail err

                                            Ok value ->
                                                loop rest <|
                                                    Dict.insert key value updates
                        in
                        loop
                            (List.map2 (\key tgiv -> ( key, tgiv.item ))
                                keys
                                otherValues
                            )
                            Dict.empty

        _ ->
            Task.fail <|
                Types.DecodeError "Wrong number of values from transactGetItems."


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


{-| A UI for entering account information.

You can certainly do this yourself, if you want it to look different,
but this gives you a good start.

The callback is called when a field changes.

You need to do OK and Cancel buttons yourself.

-}
renderAccount : (Account -> msg) -> Account -> Html msg
renderAccount callback account =
    span []
        [ b "tableName: "
        , input
            [ size 20
            , value account.tableName
            , onInput <|
                \a ->
                    callback
                        { account | tableName = a }
            ]
            []
        , br
        , b "accessKey: "
        , input
            [ size 20
            , value account.accessKey
            , onInput <|
                \a -> callback { account | accessKey = a }
            ]
            []
        , br
        , b "secretKey: "
        , input
            [ size 50
            , type_ "password"
            , value account.secretKey
            , onInput <|
                \a -> callback { account | secretKey = a }
            ]
            []
        , br
        , b "region: "
        , let
            v =
                case account.region of
                    Nothing ->
                        ""

                    Just reg ->
                        reg
          in
          input
            [ size 20
            , value v
            , onInput <|
                \a ->
                    let
                        reg =
                            if a == "" then
                                Nothing

                            else
                                Just a
                    in
                    callback { account | region = reg }
            ]
            []
        ]


{-| Merge information from `Account` into AppState.
-}
mergeAccount : Account -> AppState -> AppState
mergeAccount account appState =
    { appState | account = account }
