----------------------------------------------------------------------
--
-- Main.elm
-- Example of using the DynamoDB module.
-- Copyright (c) 2022-2025 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (..)

import Browser
import Dict exposing (Dict)
import DynamoDB
    exposing
        ( ScanValue
        , TransactGetItem
        , TransactGetItemValue
        , TransactWrite(..)
        )
import DynamoDB.AppState as AppState exposing (AppState, Updates)
import DynamoDB.EncodeDecode as ED
import DynamoDB.Html exposing (prettyTableCssClass, renderItemTable, renderTable)
import DynamoDB.Types
    exposing
        ( Account
        , AttributeValue(..)
        , Error(..)
        , Item
        , Key(..)
        , TableName
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , input
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( checked
        , class
        , cols
        , disabled
        , href
        , name
        , rows
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http exposing (Metadata)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Task
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Time.every 1000 Tick
        , view = view
        }


type alias Model =
    { time : Int
    , display : String
    , accounts : List Account
    , account : Account
    , appState : AppState
    , delayedCmd : Maybe (Cmd Msg)
    , columns : List String
    , row : Row
    , selection : Row
    , rows : List Row
    , columnName : String

    -- AppState example state above.
    -- DynamoDB example state below
    , key : Key
    , item : Maybe Item
    , text : String
    , limit : String
    , scanValue : Maybe ScanValue
    , transactKeys : String
    , transactAttributeNames : String
    , transactDeletedKeys : String
    , transactIncrement : String
    , metadata : Maybe Metadata
    }


type Msg
    = Tick Posix
    | InitAppState Posix
    | Delay Msg
    | ReceiveAppStateUpdates (Result AppState.Error (Maybe Updates))
    | ReceiveAppStateStore (Result AppState.Error Int)
    | SaveRow (Maybe String) (Maybe Row)
    | SetAccount String
    | SetSelection Row
    | SetRowColumn String String
    | AddRow
    | RemoveRow
    | UpdateRow
      -- AppState example above
      -- DynanoDB example below
    | SetKeyName String
    | SetKeyValue String
    | SetText String
    | SetLimit String
    | SetTransactKeys String
    | SetTransactAttributeNames String
    | SetTransactDeletedKeys String
    | SetTransactIncrement String
    | ClearLastEvaluatedKey
    | ClickScanItem Item
    | GetItem
    | PutItem
    | DeleteItem
    | Scan
    | TransactGetItems
    | TransactWriteItems
    | ReceiveGetItem Key (Result Error ( Metadata, Maybe Item ))
    | ReceivePutItem Key (Result Error Metadata)
    | ReceiveDeleteItem Key (Result Error Metadata)
    | ReceiveScan (Result Error ( Metadata, Maybe ScanValue ))
    | ReceiveTransactGetItems (Result Error ( Metadata, List TransactGetItemValue ))
    | ReceiveTransactWriteItems (Result Error Metadata)
    | ReceiveAccounts (Result Error (List Account))


emptyAccount : Account
emptyAccount =
    let
        account =
            DynamoDB.Types.emptyAccount
    in
    { account
        | name = "mammudeck"
        , tableName = "mammudeck"
    }


emptyAppState : AppState
emptyAppState =
    let
        appState =
            AppState.makeAppState emptyAccount
    in
    { appState
        | keyPrefix = Just "_AppStateExample"
        , idlePeriod = 2000
        , updatePeriod = 2000
    }


type alias Row =
    Dict String String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , appState = emptyAppState
      , delayedCmd = Nothing
      , columns = [ "key", "value" ]
      , row = Dict.empty
      , selection = Dict.empty
      , rows = []
      , columnName = "key"
      , key = SimpleKey ( "key", StringValue "test" )
      , item = Nothing
      , text = ""
      , limit = ""
      , scanValue = Nothing
      , transactKeys = ""
      , transactAttributeNames = ""
      , transactDeletedKeys = ""
      , transactIncrement = "1"
      , metadata = Nothing
      }
    , Cmd.batch
        [ Task.attempt ReceiveAccounts (DynamoDB.readAccounts Nothing)
        , Task.perform InitAppState Time.now
        ]
    )


getItem : Model -> ( Model, Cmd Msg )
getItem model =
    checkKey model
        (\() ->
            ( { model
                | display =
                    "Fetching " ++ keyToString model.key ++ "..."
                , item = Nothing
                , text = ""
                , metadata = Nothing
              }
            , DynamoDB.getItemWithMetadata model.account.tableName model.key
                |> DynamoDB.send model.account
                |> Task.attempt (ReceiveGetItem model.key)
            )
        )


itemToText : Item -> String
itemToText item =
    ED.encodeItem item |> JE.encode 2


receiveGetItem : Key -> Result Error ( Metadata, Maybe Item ) -> Model -> ( Model, Cmd Msg )
receiveGetItem key result model =
    case result of
        Err err ->
            let
                keystr =
                    "key: " ++ Debug.toString key
            in
            ( { model
                | display =
                    "receiveGetItem, " ++ keystr ++ ", " ++ Debug.toString err
              }
            , Cmd.none
            )

        Ok ( metadata, maybeItem ) ->
            let
                mdl =
                    { model | metadata = Just metadata }
            in
            ( case maybeItem of
                Just item ->
                    let
                        item2 =
                            DynamoDB.removeKeyFields key item
                    in
                    { mdl
                        | display = "Got: " ++ keyToString key
                        , item = Just item2
                        , text = itemToText item2
                    }

                Nothing ->
                    { mdl
                        | display = "No value for: " ++ keyToString key
                        , item = Nothing
                        , text = ""
                    }
            , Cmd.none
            )


putItem : Model -> ( Model, Cmd Msg )
putItem model =
    checkKey model
        (\() ->
            let
                mdl =
                    { model
                        | item = Nothing
                        , metadata = Nothing
                    }
            in
            case JD.decodeString ED.itemDecoder mdl.text of
                Err err ->
                    ( { mdl
                        | display = "Encode error: " ++ Debug.toString err
                      }
                    , Cmd.none
                    )

                Ok item ->
                    ( { mdl
                        | display = "Putting " ++ keyToString model.key ++ "..."
                        , item = Just item
                        , text = itemToText item
                      }
                    , DynamoDB.putItemWithMetadata model.account.tableName model.key item
                        |> DynamoDB.send model.account
                        |> Task.attempt (ReceivePutItem model.key)
                    )
        )


deleteItem : Model -> ( Model, Cmd Msg )
deleteItem model =
    checkKey model
        (\() ->
            ( { model
                | display = "Deleting " ++ keyToString model.key ++ "..."
                , metadata = Nothing
              }
            , DynamoDB.deleteItemWithMetadata model.account.tableName model.key
                |> DynamoDB.send model.account
                |> Task.attempt (ReceiveDeleteItem model.key)
            )
        )


scan : Model -> ( Model, Cmd Msg )
scan model =
    let
        limit =
            String.toInt model.limit
    in
    if model.limit /= "" && limit == Nothing then
        ( { model | display = "Limit must be blank or an integer." }
        , Cmd.none
        )

    else
        ( { model
            | display = "Scanning..."
            , scanValue = Nothing
          }
        , let
            lastEvaluatedKey =
                case model.scanValue of
                    Nothing ->
                        Nothing

                    Just scanValue ->
                        scanValue.lastEvaluatedKey
          in
          DynamoDB.scanWithMetadata model.account.tableName lastEvaluatedKey limit
            |> DynamoDB.send model.account
            |> Task.attempt ReceiveScan
        )


receiveScan : Result Error ( Metadata, Maybe ScanValue ) -> Model -> ( Model, Cmd Msg )
receiveScan result model =
    case result of
        Err err ->
            ( { model
                | display = "receiveScan: " ++ Debug.toString err
                , metadata = Nothing
              }
            , Cmd.none
            )

        Ok ( metadata, scanValue ) ->
            ( { model
                | display = "Scan successful."
                , scanValue = scanValue
                , metadata = Just metadata
              }
            , Cmd.none
            )


trimSplit : String -> List String
trimSplit string =
    String.split "," string
        |> List.map String.trim
        |> (\l ->
                if l == [ "" ] then
                    []

                else
                    l
           )


transactGetItems : Model -> ( Model, Cmd Msg )
transactGetItems model =
    let
        keys =
            trimSplit model.transactKeys

        attributes : Maybe (List String)
        attributes =
            trimSplit model.transactAttributeNames
                |> (\l ->
                        if l == [] then
                            Nothing

                        else
                            Just l
                   )

        items : List TransactGetItem
        items =
            List.map
                (\keyName ->
                    { tableName = model.account.tableName
                    , key =
                        case model.key of
                            SimpleKey ( name, _ ) ->
                                SimpleKey ( name, StringValue keyName )

                            CompositeKey ( name, _ ) pair ->
                                CompositeKey ( name, StringValue keyName ) pair
                    , returnedAttributeNames = attributes
                    }
                )
                keys
    in
    ( { model
        | display = "TransactGetItems..."
        , scanValue = Nothing
      }
    , DynamoDB.transactGetItemsWithMetadata items
        |> DynamoDB.send model.account
        |> Task.attempt ReceiveTransactGetItems
    )


transactWriteItems : Model -> ( Model, Cmd Msg )
transactWriteItems model =
    let
        transactKeys =
            trimSplit model.transactKeys

        attributes =
            trimSplit model.transactAttributeNames

        deletedKeys =
            trimSplit model.transactDeletedKeys

        incString =
            String.trim model.transactIncrement

        incNumber =
            Maybe.withDefault 0 <| String.toFloat incString

        keyName =
            primaryKeyName model.key

        kvToKey kv =
            SimpleKey ( keyName, StringValue kv )

        incAttributes item =
            Dict.map
                (\k v ->
                    if
                        (incString == "")
                            || (attributes /= [] && (not <| List.member k attributes))
                    then
                        v

                    else
                        case v of
                            StringValue s ->
                                StringValue <| s ++ "." ++ incString

                            NumberValue s ->
                                NumberValue <| s + incNumber

                            _ ->
                                v
                )
                item

        puts =
            case model.scanValue of
                Nothing ->
                    []

                Just scanValue ->
                    let
                        putPairs =
                            List.filterMap
                                (\item ->
                                    case Dict.get keyName item of
                                        Nothing ->
                                            Nothing

                                        Just v ->
                                            case v of
                                                StringValue s ->
                                                    if
                                                        List.member s transactKeys
                                                            && (not <| List.member s deletedKeys)
                                                    then
                                                        Just ( s, item )

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                )
                                scanValue.items
                    in
                    List.map
                        (\( kv, item ) ->
                            TransactWritePut
                                { tableName = model.account.tableName
                                , key = Just <| kvToKey kv
                                , item = incAttributes item
                                }
                        )
                        putPairs

        deletes =
            deletedKeys
                |> List.map
                    (\kv ->
                        TransactWriteDelete
                            { tableName = model.account.tableName
                            , key = kvToKey kv
                            }
                    )
    in
    ( { model
        | display = "TransactWriteItems..."
        , metadata = Nothing
      }
    , DynamoDB.transactWriteItemsWithMetadata (puts ++ deletes)
        |> DynamoDB.send model.account
        |> Task.attempt ReceiveTransactWriteItems
    )


receiveTransactGetItems : Result Error ( Metadata, List TransactGetItemValue ) -> Model -> ( Model, Cmd Msg )
receiveTransactGetItems result model =
    case result of
        Err err ->
            ( { model
                | display = "ReceiveTransactGetItems: " ++ Debug.toString err
                , metadata = Nothing
              }
            , Cmd.none
            )

        Ok ( metadata, getItemsValue ) ->
            let
                transactGetItemToItem : TransactGetItemValue -> Item
                transactGetItemToItem value =
                    case value.item of
                        Nothing ->
                            Dict.fromList
                                [ ( "<missing key>"
                                  , StringValue <| primaryKeyValue value.key
                                  )
                                ]

                        Just item ->
                            item
            in
            ( { model
                | display = "TransactGetItems successful."
                , scanValue =
                    Just
                        { count = List.length getItemsValue
                        , items =
                            List.map transactGetItemToItem getItemsValue
                        , lastEvaluatedKey = Nothing
                        }
              }
            , Cmd.none
            )


defaultAccount : Account
defaultAccount =
    { name = "No account"
    , region = Nothing
    , accessKey = ""
    , secretKey = ""
    , tableName = "No table"
    }


findAccount : Model -> String -> Account
findAccount model name =
    case LE.find (\a -> a.name == name) model.accounts of
        Nothing ->
            defaultAccount

        Just a ->
            a


sortRows : List Row -> List Row
sortRows rows =
    List.sortBy (\x -> Maybe.withDefault "" <| Dict.get "key" x) rows


saveRow : Maybe String -> Maybe Row -> Cmd Msg
saveRow key row =
    Task.perform (SaveRow key) <| Task.succeed row


msgCmd : Msg -> Cmd Msg
msgCmd msg =
    Task.succeed msg |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        needDelay =
            (not <| AppState.isActive model.time model.appState)
                && (case msg of
                        SaveRow _ _ ->
                            True

                        SetSelection _ ->
                            True

                        AddRow ->
                            True

                        RemoveRow ->
                            True

                        UpdateRow ->
                            True

                        _ ->
                            False
                   )
    in
    if needDelay then
        case model.delayedCmd of
            Just _ ->
                -- Should I keep a queue of delayed commands?
                -- This just drops those on the floor until
                -- the Amazon DB responds.
                ( model, Cmd.none )

            Nothing ->
                let
                    appState =
                        AppState.goActive model.time model.appState

                    mdl =
                        { model
                            | appState = appState
                        }
                in
                update (Tick <| Time.millisToPosix model.time)
                    { mdl
                        | appState = { appState | lastIdleTime = 0 }
                        , delayedCmd = Just <| msgCmd msg
                    }

    else
        case msg of
            InitAppState posix ->
                ( { model
                    | appState =
                        AppState.goActive (Time.posixToMillis posix) model.appState
                  }
                , Cmd.none
                )

            Tick posix ->
                let
                    time =
                        Time.posixToMillis posix

                    useDelayedCmd m =
                        case m.delayedCmd of
                            Nothing ->
                                ( m, Cmd.none )

                            Just c ->
                                ( { m | delayedCmd = Nothing }
                                , c
                                )

                    ( mdl, cmd ) =
                        if AppState.accountIncomplete model.appState then
                            useDelayedCmd model

                        else
                            case AppState.idle time model.appState of
                                Nothing ->
                                    if not <| AppState.isActive time model.appState then
                                        useDelayedCmd model

                                    else
                                        case AppState.update time model.appState of
                                            Nothing ->
                                                useDelayedCmd model

                                            Just ( appState, task ) ->
                                                ( { model | appState = appState }
                                                , Task.attempt ReceiveAppStateUpdates task
                                                )

                                Just ( appState, task ) ->
                                    ( { model | appState = appState }
                                    , Task.attempt
                                        ReceiveAppStateStore
                                        task
                                    )
                in
                ( { mdl | time = time }
                , cmd
                )

            Delay msg2 ->
                ( model, msgCmd msg2 )

            ReceiveAppStateUpdates result ->
                let
                    ( mdl, cmd ) =
                        case model.delayedCmd of
                            Nothing ->
                                ( model, Cmd.none )

                            Just c ->
                                ( { model | delayedCmd = Nothing }, c )
                in
                case result of
                    Err err ->
                        ( { mdl
                            | display =
                                "ReceiveAppStateUpdates: " ++ Debug.toString err
                          }
                        , cmd
                        )

                    Ok Nothing ->
                        ( mdl, cmd )

                    Ok (Just updates) ->
                        let
                            appState =
                                mdl.appState

                            folder k mv rowsTail =
                                case mv of
                                    Nothing ->
                                        rowsTail

                                    Just v ->
                                        Dict.fromList [ ( "key", k ), ( "value", v ) ]
                                            :: rowsTail

                            oldRows =
                                List.filter
                                    (\row ->
                                        case Dict.get "key" row of
                                            Nothing ->
                                                True

                                            Just k ->
                                                Nothing
                                                    == Dict.get k updates.updates
                                    )
                                    mdl.rows

                            rows =
                                Dict.foldr folder oldRows updates.updates
                        in
                        ( { mdl
                            | display =
                                if model.appState.saveCount == 0 then
                                    "InitialLoad received."

                                else
                                    "Updates received."
                            , appState =
                                { appState
                                    | saveCount = updates.saveCount
                                    , keyCounts = updates.keyCounts
                                }
                            , rows = sortRows rows
                          }
                        , cmd
                        )

            ReceiveAppStateStore result ->
                case result of
                    Err err ->
                        ( { model
                            | display =
                                "RecieveAppStateStore: " ++ Debug.toString err
                          }
                        , Cmd.none
                        )

                    Ok count ->
                        if count == 0 then
                            -- Nothing actually done
                            ( model, Cmd.none )

                        else
                            ( { model
                                | display =
                                    "Saved " ++ String.fromInt count ++ " items."
                              }
                            , Cmd.none
                            )

            SaveRow key row ->
                case key of
                    Nothing ->
                        ( model, Cmd.none )

                    Just k ->
                        let
                            val =
                                case row of
                                    Nothing ->
                                        Nothing

                                    Just mv ->
                                        case Dict.get "value" mv of
                                            Nothing ->
                                                Nothing

                                            Just v ->
                                                Just <| JE.string v
                        in
                        if AppState.accountIncomplete model.appState then
                            ( { model
                                | display =
                                    "Can't save to DynamoDB: account incomplete:"
                                        ++ Debug.toString model.appState
                              }
                            , Cmd.none
                            )

                        else
                            case AppState.save model.time k val model.appState of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just ( appState, task ) ->
                                    ( { model | appState = appState }
                                    , Task.attempt ReceiveAppStateStore task
                                    )

            SetAccount name ->
                let
                    account =
                        findAccount model name

                    appState =
                        model.appState
                in
                ( { model
                    | account = account
                    , display = "Account: " ++ name
                    , appState = { appState | account = account }
                  }
                , Cmd.none
                )

            SetSelection row ->
                ( { model
                    | selection = row
                    , row = row
                  }
                , Cmd.none
                )

            SetRowColumn columnName v ->
                let
                    row =
                        if v == "" then
                            Dict.remove columnName model.row

                        else
                            Dict.insert columnName v model.row
                in
                ( { model | row = row }
                , Cmd.none
                )

            AddRow ->
                let
                    keyValue =
                        Dict.get "key" model.row
                in
                case LE.find (\row -> Dict.get "key" row == keyValue) model.rows of
                    Just _ ->
                        update UpdateRow model

                    Nothing ->
                        ( { model
                            | selection = model.row
                            , rows =
                                model.rows ++ [ model.row ] |> sortRows
                          }
                        , saveRow keyValue <| Just model.row
                        )

            RemoveRow ->
                let
                    keyValue =
                        Dict.get "key" model.row
                in
                ( { model
                    | selection =
                        case keyValue of
                            Nothing ->
                                Dict.empty

                            _ ->
                                model.selection
                    , rows = LE.filterNot ((==) keyValue << Dict.get "key") model.rows
                  }
                , saveRow keyValue Nothing
                )

            UpdateRow ->
                case Dict.get "key" model.row of
                    Nothing ->
                        ( model, Cmd.none )

                    keyValue ->
                        ( { model
                            | selection = model.row
                            , rows =
                                List.map
                                    (\row ->
                                        if Dict.get "key" row == keyValue then
                                            model.row

                                        else
                                            row
                                    )
                                    model.rows
                                    |> sortRows
                          }
                        , saveRow keyValue <| Just model.row
                        )

            SetKeyName name ->
                let
                    key =
                        case model.key of
                            SimpleKey ( k, v ) ->
                                SimpleKey ( name, v )

                            x ->
                                x
                in
                ( { model | key = key }
                , Cmd.none
                )

            SetKeyValue value ->
                let
                    key =
                        case model.key of
                            SimpleKey ( k, v ) ->
                                SimpleKey ( k, StringValue value )

                            x ->
                                x
                in
                ( { model | key = key }
                , Cmd.none
                )

            SetText text ->
                ( { model | text = text }
                , Cmd.none
                )

            SetLimit text ->
                ( { model | limit = text }
                , Cmd.none
                )

            SetTransactKeys text ->
                ( { model | transactKeys = text }
                , Cmd.none
                )

            SetTransactAttributeNames text ->
                ( { model | transactAttributeNames = text }
                , Cmd.none
                )

            SetTransactDeletedKeys text ->
                ( { model | transactDeletedKeys = text }
                , Cmd.none
                )

            SetTransactIncrement text ->
                ( { model | transactIncrement = text }
                , Cmd.none
                )

            ClearLastEvaluatedKey ->
                case model.scanValue of
                    Nothing ->
                        ( model, Cmd.none )

                    Just v ->
                        ( { model
                            | scanValue =
                                Just { v | lastEvaluatedKey = Nothing }
                          }
                        , Cmd.none
                        )

            ClickScanItem item ->
                let
                    keyName =
                        case model.key of
                            SimpleKey ( name, _ ) ->
                                name

                            _ ->
                                ""

                    mdl =
                        { model
                            | item = Nothing
                            , text = ""
                        }
                in
                if keyName == "" then
                    ( { mdl | display = "\"Key name\" is blank." }
                    , Cmd.none
                    )

                else
                    case Dict.get keyName item of
                        Nothing ->
                            ( { mdl
                                | display =
                                    "Clicked item has no attribute named \""
                                        ++ keyName
                                        ++ "\"."
                              }
                            , Cmd.none
                            )

                        Just value ->
                            case value of
                                StringValue _ ->
                                    let
                                        key =
                                            SimpleKey ( keyName, value )
                                    in
                                    ( { mdl
                                        | key = key
                                        , item = Just item
                                        , text =
                                            DynamoDB.removeKeyFields key item
                                                |> itemToText
                                        , display = "Row ready for editing."
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( { mdl
                                        | display =
                                            "Non-string value for attribute named \""
                                                ++ keyName
                                                ++ "\"."
                                      }
                                    , Cmd.none
                                    )

            GetItem ->
                getItem model

            PutItem ->
                putItem model

            DeleteItem ->
                deleteItem model

            Scan ->
                scan model

            TransactGetItems ->
                transactGetItems model

            TransactWriteItems ->
                transactWriteItems model

            ReceiveGetItem key result ->
                receiveGetItem key result model

            ReceivePutItem key result ->
                receiveEmptyResult "Put" (Just key) result model

            ReceiveDeleteItem key result ->
                receiveEmptyResult "Deleted" (Just key) result model

            ReceiveScan result ->
                receiveScan result model

            ReceiveTransactGetItems result ->
                receiveTransactGetItems result model

            ReceiveTransactWriteItems result ->
                receiveEmptyResult "TransactWriteItems successful." Nothing result model

            ReceiveAccounts result ->
                case result of
                    Err err ->
                        ( { model | display = "ReceiveAccounts: " ++ Debug.toString err }
                        , Cmd.none
                        )

                    Ok accounts ->
                        let
                            account =
                                case accounts of
                                    a :: _ ->
                                        a

                                    _ ->
                                        defaultAccount

                            appState =
                                model.appState
                        in
                        ( { model
                            | accounts = accounts
                            , account = account
                            , appState = { appState | account = account }
                            , display = "Accounts received."
                          }
                        , Cmd.none
                        )


checkKey : Model -> (() -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
checkKey model thunk =
    let
        name =
            case model.key of
                SimpleKey ( n, _ ) ->
                    n

                _ ->
                    ""
    in
    if name == "" then
        ( { model | display = "Blank key." }
        , Cmd.none
        )

    else
        thunk ()


receiveEmptyResult : String -> Maybe Key -> Result Error Metadata -> Model -> ( Model, Cmd Msg )
receiveEmptyResult label maybeKey result model =
    case result of
        Err err ->
            ( { model | display = "receiveEmptyResult: " ++ Debug.toString err }
            , Cmd.none
            )

        Ok metadata ->
            let
                mdl =
                    { model | metadata = Just metadata }
            in
            ( { mdl
                | display =
                    case maybeKey of
                        Nothing ->
                            label

                        Just key ->
                            label ++ ":" ++ keyToString key
              }
            , Cmd.none
            )


keyToString : Key -> String
keyToString key =
    JE.encode 0 <| ED.encodeKey key


primaryKeyName : Key -> String
primaryKeyName key =
    case key of
        SimpleKey ( name, _ ) ->
            name

        CompositeKey ( name, _ ) _ ->
            name


attributeValueToString : AttributeValue -> String
attributeValueToString value =
    case value of
        StringValue s ->
            s

        _ ->
            JE.encode 0 <| ED.encodeAttributeValue value


primaryKeyValue : Key -> String
primaryKeyValue key =
    case key of
        SimpleKey ( _, value ) ->
            attributeValueToString value

        CompositeKey ( _, value ) _ ->
            attributeValueToString value


h1 : String -> Html msg
h1 s =
    Html.h1 [] [ text s ]


h2 : String -> Html msg
h2 s =
    Html.h2 [] [ text s ]


view : Model -> Html Msg
view model =
    div
        [ style "margin-left" "3em"
        ]
        [ prettyTableCssClass
        , h1 "DynamoDB Example"
        , p [ style "color" "red" ]
            [ text model.display ]
        , p []
            [ b "Account: "
            , accountSelector model
            , br
            , b "Table Name: "
            , text model.account.tableName
            ]
        , viewAppStateExample model
        , viewDynamoDBExample model
        ]


rowTable : Row -> Model -> Html Msg
rowTable row model =
    let
        columnTh columnName =
            th [] [ text columnName ]

        columnTd columnName =
            let
                v =
                    case Dict.get columnName model.row of
                        Nothing ->
                            ""

                        Just s ->
                            s
            in
            td []
                [ input
                    [ value v
                    , size 20
                    , onInput <| SetRowColumn columnName
                    ]
                    []
                ]
    in
    table [ class "prettytable" ] <|
        tr [] (List.map columnTh model.columns)
            :: List.map columnTd model.columns


viewAppStateExample : Model -> Html Msg
viewAppStateExample model =
    let
        addSelectionMarker row =
            if row == model.selection then
                Dict.insert "*" "*" row

            else
                row
    in
    div [ style "margin" "8px" ]
        [ h2 "AppState Example"
        , p []
            [ rowTable model.row model
            , button [ onClick AddRow ]
                [ text "Add" ]
            , text " "
            , button [ onClick RemoveRow ]
                [ text "Remove" ]
            , text " "
            , button [ onClick UpdateRow ]
                [ text "Update" ]
            ]
        , p []
            [ renderTable SetSelection
                { tableConfig | columnDescriptors = "*" :: model.columns }
                (List.map addSelectionMarker model.rows)
            ]
        ]


tableConfig =
    { columnDescriptors = []
    , columnDescriptorToString = identity
    , elementToString = getRowValue
    }


getRowValue : String -> Row -> Maybe String
getRowValue column row =
    Dict.get column row


viewDynamoDBExample : Model -> Html Msg
viewDynamoDBExample model =
    span []
        [ h2 "DynamoDB Example"
        , p []
            [ b "Key name: "
            , input
                [ type_ "text"
                , size 40
                , value <| primaryKeyName model.key
                , onInput SetKeyName
                ]
                []
            , br
            , b "Key value: "
            , input
                [ type_ "text"
                , size 40
                , value <| primaryKeyValue model.key
                , onInput SetKeyValue
                ]
                []
            ]
        , p []
            [ button [ onClick GetItem ]
                [ text "GetItem" ]
            , text " "
            , button [ onClick DeleteItem ]
                [ text "DeleteItem" ]
            ]
        , p []
            [ textarea
                [ cols 80
                , rows 20
                , value model.text
                , onInput SetText
                ]
                []
            , br
            , text "Fill in the text box above with JSON for an object each of whose values is a JSON object with a key of one of the DynamoDB types (e.g. \"N\" for number or \"S\" for string), and a value of the string encoding of the value)."
            , br
            , text "Example: {\"count\":{\"N\":\"42\"},\"value\":{\"S\":\"test-value\"}}"
            , br
            , button [ onClick PutItem ]
                [ text "PutItem" ]
            ]
        , case model.item of
            Nothing ->
                text ""

            Just item ->
                p []
                    [ text "The Elm for the JSON in the text box above."
                    , br
                    , b "Item: "
                    , text <| Debug.toString (Dict.toList item)
                    ]
        , p []
            [ b "Limit: "
            , input
                [ type_ "text"
                , size 5
                , value model.limit
                , onInput SetLimit
                ]
                []
            , text " "
            , button [ onClick Scan ]
                [ text "Scan" ]
            ]
        , case model.scanValue of
            Nothing ->
                text ""

            Just scanValue ->
                p []
                    [ b "scan count: "
                    , text <| String.fromInt scanValue.count
                    , br
                    , b "lastEvaluatedKey: "
                    , text <| Debug.toString scanValue.lastEvaluatedKey
                    , if scanValue.lastEvaluatedKey == Nothing then
                        text " "

                      else
                        span []
                            [ text " "
                            , button [ onClick ClearLastEvaluatedKey ]
                                [ text "Clear" ]
                            ]
                    , br
                    , text "Click a row below to select it for editing."
                    , renderItemTable ClickScanItem
                        (DynamoDB.keyNames model.key)
                        scanValue.items
                    ]
        , p []
            [ text "The following two fields are comma-separated-lists."
            , br
            , text "Leave \"Tranaction attributes\" blank to get all attributes."
            , br
            , b "Transaction keys: "
            , input
                [ type_ "text"
                , size 40
                , value model.transactKeys
                , onInput SetTransactKeys
                ]
                []
            , br
            , b "Transaction attributes: "
            , input
                [ type_ "text"
                , size 40
                , value model.transactAttributeNames
                , onInput SetTransactAttributeNames
                ]
                []
            , br
            , button [ onClick TransactGetItems ]
                [ text "TransactGetItems" ]
            ]
        , p []
            [ b "Transaction deleted keys: "
            , input
                [ type_ "text"
                , size 40
                , value model.transactDeletedKeys
                , onInput SetTransactDeletedKeys
                ]
                []
            , br
            , b "Transaction increment: "
            , input
                [ type_ "text"
                , size 10
                , value model.transactIncrement
                , onInput SetTransactIncrement
                ]
                []
            , br
            , text <|
                "The \"TransactWriteItems\" button operates on rows whose key values"
                    ++ " are listed in \"Transaction keys\" or \"Transaction deleted keys\""
                    ++ "and on the attributes in \"Transaction attributes\"."
            , br
            , text <|
                "It adds the \"Transaction increment\" to the values displayed in"
                    ++ " the table populated by the \"Scan\" or \"TransactGetItems\" button."
            , br
            , button [ onClick TransactWriteItems ]
                [ text "TransactWriteItems" ]
            ]
        , case model.metadata of
            Nothing ->
                text ""

            Just metadata ->
                p []
                    [ b "URL: "
                    , text metadata.url
                    , br
                    , b "statusCode: "
                    , text <| String.fromInt metadata.statusCode
                    , br
                    , b "statusText: "
                    , text metadata.statusText
                    , br
                    , b "Headers: "
                    , text <| Debug.toString (Dict.toList metadata.headers)
                    ]
        ]


b : String -> Html msg
b str =
    Html.b [] [ text str ]


br : Html msg
br =
    Html.br [] []


accountSelector : Model -> Html Msg
accountSelector model =
    select [ on "change" (JD.map SetAccount targetValue) ]
        (List.map (accountOption model) model.accounts)


accountOption : Model -> Account -> Html Msg
accountOption model account =
    option
        [ value account.name
        , selected (model.account.name == account.name)
        ]
        [ text account.name ]


thText : String -> Html Msg
thText string =
    th [] [ text string ]


tdAlignHtml : String -> Html Msg -> Html Msg
tdAlignHtml alignment html =
    td
        [ style "padding-left" "1em"
        , style "padding-right" "1em"
        , style "text-align" alignment
        ]
        [ html ]


tdAlignText : String -> String -> Html Msg
tdAlignText alignment string =
    tdAlignHtml alignment <| text string


tdHtml : Html Msg -> Html Msg
tdHtml html =
    tdAlignHtml "left" html


tdText : String -> Html Msg
tdText string =
    tdAlignText "left" string
