----------------------------------------------------------------------
--
-- Main.elm
-- Example of using the DynamoDB module.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (..)

import Browser
import Dict exposing (Dict)
import DynamoDB exposing (ScanValue, TransactGetItem, TransactGetItemValue)
import DynamoDB.EncodeDecode as ED
import DynamoDB.Html exposing (renderItemTable)
import DynamoDB.Types
    exposing
        ( Account
        , AttributeValue(..)
        , Error(..)
        , Item
        , Key(..)
        , QueryElement(..)
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
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


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { display : String
    , accounts : List Account
    , account : Account
    , key : Key
    , item : Maybe Item
    , text : String
    , limit : String
    , scanValue : Maybe ScanValue
    , transactKeys : String
    , transactAttributeNames : String
    , metadata : Maybe Metadata
    }


type Msg
    = SetAccount String
    | SetKeyName String
    | SetKeyValue String
    | SetText String
    | SetLimit String
    | SetTransactKeys String
    | SetTransactAttributeNames String
    | ClearLastEvaluatedKey
    | ClickScanItem Item
    | GetItem
    | PutItem
    | DeleteItem
    | Scan
    | TransactGetItems
    | ReceiveGetItem Key (Result Error ( Metadata, Maybe Item ))
    | ReceivePutItem Key (Result Error Metadata)
    | ReceiveDeleteItem Key (Result Error Metadata)
    | ReceiveScan (Result Error ( Metadata, Maybe ScanValue ))
    | ReceiveTransactGetItems (Result Error ( Metadata, List TransactGetItemValue ))
    | ReceiveAccounts (Result Error (List Account))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , key = SimpleKey ( "key", StringValue "test" )
      , item = Nothing
      , text = ""
      , limit = ""
      , scanValue = Nothing
      , transactKeys = ""
      , transactAttributeNames = ""
      , metadata = Nothing
      }
    , Task.attempt ReceiveAccounts (DynamoDB.readAccounts Nothing)
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
            ( { model | display = Debug.toString err }
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
                | display = Debug.toString err
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


receiveTransactGetItems : Result Error ( Metadata, List TransactGetItemValue ) -> Model -> ( Model, Cmd Msg )
receiveTransactGetItems result model =
    case result of
        Err err ->
            ( { model
                | display = Debug.toString err
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAccount name ->
            let
                account =
                    findAccount model name
            in
            ( { model
                | account = account
                , display = "Account: " ++ name
              }
            , Cmd.none
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

        ReceiveAccounts result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
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
                    in
                    ( { model
                        | accounts = accounts
                        , account = account
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
            ( { model | display = Debug.toString err }
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


view : Model -> Html Msg
view model =
    div
        [ style "margin-left" "3em"
        ]
        [ p [ style "color" "red" ]
            [ text model.display ]
        , p []
            [ text "Account: "
            , accountSelector model
            , br
            , text "Table Name: "
            , text model.account.tableName
            ]
        , p []
            [ text "Key name: "
            , input
                [ type_ "text"
                , size 40
                , value <| primaryKeyName model.key
                , onInput SetKeyName
                ]
                []
            , br
            , text "Key value: "
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
            , button [ onClick PutItem ]
                [ text "PutItem" ]
            ]
        , case model.item of
            Nothing ->
                text ""

            Just item ->
                p []
                    [ text "Item: "
                    , text <| Debug.toString (Dict.toList item)
                    ]
        , p []
            [ text "Limit: "
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
                    [ text "scan count: "
                    , text <| String.fromInt scanValue.count
                    , br
                    , text "lastEvaluatedKey: "
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
            , text "Transaction keys: "
            , input
                [ type_ "text"
                , size 40
                , value model.transactKeys
                , onInput SetTransactKeys
                ]
                []
            , br
            , text "Transaction attributes: "
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
        , case model.metadata of
            Nothing ->
                text ""

            Just metadata ->
                p []
                    [ text "URL: "
                    , text metadata.url
                    , br
                    , text "statusCode: "
                    , text <| String.fromInt metadata.statusCode
                    , br
                    , text "statusText: "
                    , text metadata.statusText
                    , br
                    , text "Headers: "
                    , text <| Debug.toString (Dict.toList metadata.headers)
                    ]
        ]


br : Html Msg
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
