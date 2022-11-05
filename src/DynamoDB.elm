--------------------------------------------------------------------
--
-- DynamoDB.elm
-- Elm client library for Amazon's DynamoDB NoSql database service.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module DynamoDB exposing
    ( Request
    , getItem, getItemWithMetadata
    , putItem, putItemWithMetadata
    , deleteItem, deleteItemWithMetadata
    , ScanValue, scan, scanWithMetadata
    , TransactGetItem, TransactGetItemValue, transactGetItems, transactGetItemsWithMetadata
    , send
    , itemStringValue, itemFloatValue, itemIntValue
    , removeKeyFields, keyNames
    , readAccounts, decodeAccounts, accountDecoder, encodeAccount
    , makeRequest, makeFullRequest
    )

{-| Pure Elm client for the [AWS DynamoDB](https://aws.amazon.com/dynamodb/) NoSQL database service.


# Types

@docs Request


# Creating requests

@docs getItem, getItemWithMetadata
@docs putItem, putItemWithMetadata
@docs deleteItem, deleteItemWithMetadata
@docs ScanValue, scan, scanWithMetadata
@docs TransactGetItem, TransactGetItemValue, transactGetItems, transactGetItemsWithMetadata


# Turning a Request into a Task

@docs send


# Accessing values in items

@docs itemStringValue, itemFloatValue, itemIntValue


# Utility functions

@docs removeKeyFields, keyNames


# Reading accounts into Elm

@docs readAccounts, decodeAccounts, accountDecoder, encodeAccount


# Low-level functions

@docs makeRequest, makeFullRequest

-}

import AWS.Config as Config exposing (Endpoint(..))
import AWS.Credentials
    exposing
        ( Credentials
        , fromAccessKeys
        )
import AWS.Http
    exposing
        ( AWSAppError
        , Body
        , Method(..)
        , Path
        , emptyBody
        )
import AWS.Service as Service exposing (Service)
import Dict exposing (Dict)
import DynamoDB.EncodeDecode as ED
import DynamoDB.Types as Types
    exposing
        ( Account
        , AttributeValue(..)
        , Error(..)
        , Item
        , Key(..)
        , Query
        , QueryElement(..)
        , TableName
        )
import Http exposing (Metadata)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Task exposing (Task)


defaultAccountsUrl : String
defaultAccountsUrl =
    "accounts.json"


{-| Read JSON from a URL and turn it into a list of `Account`s.

If `Nothing` is passed for the first arg (the URL), will use the default of `"accounts.json"`.

You're not going to want to store the secret keys in this JSON in plain text anywhere but your development machine. In applications, they will ususally be stored in `LocalStorage`.

Example JSON:

    [{"name": "Dynamo DB",
      "region": "us-east-1",
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "tableName": "<your table name>"
     }
    ]

-}
readAccounts : Maybe String -> Task Error (List Account)
readAccounts maybeUrl =
    let
        url =
            case maybeUrl of
                Just u ->
                    u

                Nothing ->
                    defaultAccountsUrl

        getTask =
            getStringTask url
    in
    Task.andThen decodeAccountsTask <|
        Task.onError handleHttpError getTask


getStringTask : String -> Task Http.Error String
getStringTask url =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver stringResponseToResult
        , timeout = Nothing
        }


stringResponseToResult : Http.Response String -> Result Http.Error String
stringResponseToResult response =
    case response of
        Http.BadUrl_ s ->
            Err <| Http.BadUrl s

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err <| Http.BadStatus metadata.statusCode

        Http.GoodStatus_ _ body ->
            Ok body


decodeAccountsTask : String -> Task Error (List Account)
decodeAccountsTask json =
    case decodeAccounts json of
        Ok accounts ->
            Task.succeed accounts

        Err error ->
            Task.fail error


handleHttpError : Http.Error -> Task Error String
handleHttpError error =
    Task.fail <| HttpError error


makeCredentials : Account -> Credentials
makeCredentials account =
    fromAccessKeys account.accessKey account.secretKey


{-| Encode an account as a JSON value.
-}
encodeAccount : Account -> JE.Value
encodeAccount account =
    JE.object <|
        List.concat
            [ [ ( "name", JE.string account.name ) ]
            , case account.region of
                Nothing ->
                    []

                Just region ->
                    [ ( "region", JE.string region ) ]
            , [ ( "access-key", JE.string account.accessKey )
              , ( "secret-key", JE.string account.secretKey )
              , ( "tableName", JE.string account.tableName )
              ]
            ]


{-| A `Decoder` for the `Account` type.
-}
accountDecoder : Decoder Account
accountDecoder =
    JD.map5 Account
        (JD.field "name" JD.string)
        (JD.oneOf
            [ JD.field "region" (JD.nullable JD.string)
            , JD.succeed Nothing
            ]
        )
        (JD.field "access-key" JD.string)
        (JD.field "secret-key" JD.string)
        (JD.field "tableName" JD.string)


accountsDecoder : Decoder (List Account)
accountsDecoder =
    JD.list accountDecoder


{-| Decode a JSON string encoding a list of `Account`s
-}
decodeAccounts : String -> Result Error (List Account)
decodeAccounts json =
    case JD.decodeString accountsDecoder json of
        Err s ->
            Err <| DecodeError (JD.errorToString s)

        Ok accounts ->
            Ok accounts


endpointPrefix : String
endpointPrefix =
    "DynamoDB"


apiVersion : Config.ApiVersion
apiVersion =
    "20120810"


protocol : Config.Protocol
protocol =
    Config.JSON


{-| Make an `AWS.Service.Service` for a `DynamoDB.Account`.

Sometimes useful for the `hostResolver`.

-}
makeService : Account -> Service
makeService { region } =
    let
        -- If this is not lower-case, we get a signing scope error.
        prefix =
            String.toLower endpointPrefix

        config =
            Config.withTargetPrefix (endpointPrefix ++ "_" ++ apiVersion) <|
                case region of
                    Nothing ->
                        Config.defineGlobal
                            prefix
                            apiVersion
                            protocol
                            Config.SignV4

                    Just reg ->
                        Config.defineRegional
                            prefix
                            apiVersion
                            protocol
                            Config.SignV4
                            reg
    in
    Service.service config


{-| A request that can be turned into a Task by `DynamoDB.send`.

`a` is the type of the successful `Task` result from `DynamoDB.send`.

-}
type alias Request a =
    AWS.Http.Request AWSAppError a


getItemWithMetadataDecoder : a -> Decoder ( a, Maybe Item )
getItemWithMetadataDecoder metadata =
    JD.maybe (JD.field "Item" JD.value)
        |> JD.andThen
            (\value ->
                case value of
                    Nothing ->
                        JD.succeed ( metadata, Nothing )

                    Just _ ->
                        (JD.map Just <|
                            JD.field "Item" ED.itemDecoder
                        )
                            |> JD.andThen
                                (\item -> JD.succeed ( metadata, item ))
            )


tossMetadataDecoder : a -> (a -> Decoder ( a, b )) -> Decoder b
tossMetadataDecoder metadata decoder =
    decoder metadata
        |> JD.andThen
            (\( _, v ) -> JD.succeed v)


getItemDecoder : Metadata -> Decoder (Maybe Item)
getItemDecoder metadata =
    tossMetadataDecoder metadata getItemWithMetadataDecoder


{-| Get an item from a table.
-}
getItem : TableName -> Key -> Request (Maybe Item)
getItem =
    getItemInternal getItemDecoder


{-| Get an item, returning the Http request `Metadata`.
-}
getItemWithMetadata : TableName -> Key -> Request ( Metadata, Maybe Item )
getItemWithMetadata =
    getItemInternal getItemWithMetadataDecoder


getItemInternal : (Metadata -> Decoder a) -> TableName -> Key -> Request a
getItemInternal decoder tableName key =
    let
        payload =
            JE.object
                [ ( "TableName", JE.string tableName )
                , ( "Key", ED.encodeKey key )
                ]
    in
    makeFullRequest "GetItem" payload decoder


voidWithMetadataDecoder : a -> Decoder a
voidWithMetadataDecoder metadata =
    JD.succeed metadata


voidDecoder : a -> Decoder ()
voidDecoder _ =
    JD.succeed ()


{-| Put an item into a table. There is no return value.

See <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html>

-}
putItem : TableName -> Key -> Item -> Request ()
putItem =
    putItemInternal voidDecoder


{-| Put an item into a table, returning the Http request `Metadata`.
-}
putItemWithMetadata : TableName -> Key -> Item -> Request Metadata
putItemWithMetadata =
    putItemInternal voidWithMetadataDecoder


putItemInternal : (Metadata -> Decoder a) -> TableName -> Key -> Item -> Request a
putItemInternal decoder tableName key item =
    let
        payload =
            JE.object
                [ ( "TableName", JE.string tableName )
                , ( "Item", addKeyFields key item |> ED.encodeItem )
                ]
    in
    makeFullRequest "PutItem" payload decoder


{-| Delete an item from a table. There is no return value.

See <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DeleteItem.html>

-}
deleteItem : TableName -> Key -> Request ()
deleteItem =
    deleteItemInternal voidDecoder


{-| Delete an item from a table, returning the Http request `Metadata`.
-}
deleteItemWithMetadata : TableName -> Key -> Request Metadata
deleteItemWithMetadata =
    deleteItemInternal voidWithMetadataDecoder


deleteItemInternal : (Metadata -> Decoder a) -> TableName -> Key -> Request a
deleteItemInternal decoder tableName key =
    let
        payload =
            JE.object
                [ ( "TableName", JE.string tableName )
                , ( "Key", ED.encodeKey key )
                ]
    in
    makeFullRequest "DeleteItem" payload decoder


{-| The return value for `scan` and `scanWithMetadata`

Pass `lastEvaluatedKey` to a subsequent call to scan to get more.

This doesn't include `ScannedCount`, because that's the same as
`Count` unless you do filtering, which isn't yet supported.

-}
type alias ScanValue =
    { count : Int
    , items : List Item
    , lastEvaluatedKey : Maybe Key
    }


scanWithMetadataDecoder : a -> Decoder ( a, Maybe ScanValue )
scanWithMetadataDecoder metadata =
    JD.maybe (JD.field "Items" JD.value)
        |> JD.andThen
            (\value ->
                case value of
                    Nothing ->
                        JD.succeed ( metadata, Nothing )

                    Just _ ->
                        JD.map3 ScanValue
                            (JD.field "Count" <| JD.int)
                            (JD.field "Items" <| JD.list ED.itemDecoder)
                            (JD.maybe <| JD.field "LastEvaluatedKey" ED.keyDecoder)
                            |> JD.andThen
                                (\res -> JD.succeed ( metadata, Just res ))
            )


scanDecoder : a -> Decoder (Maybe ScanValue)
scanDecoder metadata =
    scanWithMetadataDecoder metadata
        |> JD.andThen
            (\( _, value ) ->
                JD.succeed value
            )


{-| Scan a table for items.

If `Maybe Key` isn't `Nothing`, it's the `ExclusiveStartKey`.

If `Maybe Int` isn't `Nothing`, it's the `Limit`.

See <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Scan.html>

-}
scan : TableName -> Maybe Key -> Maybe Int -> Request (Maybe ScanValue)
scan =
    scanInternal scanDecoder


{-| Scan a table for items, returning the Http request `Metadata`.

If `Maybe Key` isn't `Nothing`, it's the `ExclusiveStartKey`.

If `Maybe Int` isn't `Nothing`, it's the `Limit`.

-}
scanWithMetadata : TableName -> Maybe Key -> Maybe Int -> Request ( Metadata, Maybe ScanValue )
scanWithMetadata =
    scanInternal scanWithMetadataDecoder


scanInternal : (Metadata -> Decoder a) -> TableName -> Maybe Key -> Maybe Int -> Request a
scanInternal decoder tableName maybeExclusiveStartKey maybeLimit =
    let
        payload =
            JE.object <|
                List.concat
                    [ [ ( "TableName", JE.string tableName ) ]
                    , case maybeExclusiveStartKey of
                        Nothing ->
                            []

                        Just key ->
                            [ ( "ExclusiveStartKey", ED.encodeKey key ) ]
                    , case maybeLimit of
                        Nothing ->
                            []

                        Just limit ->
                            [ ( "Limit", JE.int limit ) ]
                    ]
    in
    makeFullRequest "Scan" payload decoder


{-| Add key fields to an `Item`.
-}
addKeyFields : Key -> Item -> Item
addKeyFields key item =
    case key of
        SimpleKey ( name, value ) ->
            Dict.insert name value item

        CompositeKey ( name1, value1 ) ( name2, value2 ) ->
            Dict.insert name1 value1 item
                |> Dict.insert name2 value2


{-| Specify one item for `transactGetItems`.

All attributes are returned if `returnedAttributeNames` is `Nothing`.

-}
type alias TransactGetItem =
    { tableName : TableName
    , key : Key
    , returnedAttributeNames : Maybe (List String)
    }


{-| One of the return values from `transactGetItems`.
-}
type alias TransactGetItemValue =
    { tableName : TableName
    , key : Key
    , item : Maybe Item
    }


encodeProjectionExpression : List String -> ( List ( String, Value ), String )
encodeProjectionExpression names =
    let
        folder : String -> ( Int, List ( String, Value ), List String ) -> ( Int, List ( String, Value ), List String )
        folder name ( idx, map, mappedNames ) =
            let
                mappedName =
                    "#" ++ String.fromInt idx
            in
            ( idx + 1
            , ( mappedName, JE.string name ) :: map
            , mappedName :: mappedNames
            )

        ( _, expressionAttributeNames, projectionExpressionNames ) =
            List.foldr folder ( 1, [], [] ) names
    in
    ( List.reverse expressionAttributeNames
    , String.join "," <| List.reverse projectionExpressionNames
    )


projectionExpressionMap : Maybe (List String) -> List ( String, Value )
projectionExpressionMap maybeNames =
    case maybeNames of
        Nothing ->
            []

        Just names ->
            let
                ( expressionAttributeNames, projectionExpression ) =
                    encodeProjectionExpression names
            in
            [ ( "ExpressionAttributeNames"
              , JE.object expressionAttributeNames
              )
            , ( "ProjectionExpression"
              , JE.string projectionExpression
              )
            ]


transactGetItemsWithMetadataDecoder : List TransactGetItem -> a -> Decoder ( a, List TransactGetItemValue )
transactGetItemsWithMetadataDecoder queries metadata =
    JD.field "Responses"
        (JD.list
            (JD.value
                |> JD.andThen
                    (\v ->
                        case JD.decodeValue (JD.field "Item" JD.value) v of
                            Err _ ->
                                -- Missing key
                                JD.succeed Nothing

                            Ok _ ->
                                JD.field "Item" ED.itemDecoder
                                    |> JD.map Just
                    )
            )
            |> JD.andThen
                (\items ->
                    let
                        values =
                            List.map2
                                (\{ tableName, key } item ->
                                    TransactGetItemValue tableName key item
                                )
                                queries
                                items
                    in
                    JD.succeed ( metadata, values )
                )
        )


transactGetItemsDecoder : List TransactGetItem -> a -> Decoder (List TransactGetItemValue)
transactGetItemsDecoder queries metadata =
    tossMetadataDecoder metadata (transactGetItemsWithMetadataDecoder queries)


{-| Do a bunch of GetItem requests inside a transaction.

Will error if the list has more than ten elements.

-}
transactGetItems : List TransactGetItem -> Request (List TransactGetItemValue)
transactGetItems =
    transactGetItemsInternal transactGetItemsDecoder


{-| Do a bunch of GetItem requests inside a transaction, returning the Http request `Metadata`.

Will error if the list has more than ten elements.

-}
transactGetItemsWithMetadata : List TransactGetItem -> Request ( Metadata, List TransactGetItemValue )
transactGetItemsWithMetadata =
    transactGetItemsInternal transactGetItemsWithMetadataDecoder


transactGetItemsInternal : (List TransactGetItem -> Metadata -> Decoder a) -> List TransactGetItem -> Request a
transactGetItemsInternal decoder getItems =
    let
        encodeGet { tableName, key, returnedAttributeNames } =
            [ ( "Get"
              , JE.object <|
                    List.concat
                        [ [ ( "Key", ED.encodeKey key )
                          , ( "TableName", JE.string tableName )
                          ]
                        , projectionExpressionMap returnedAttributeNames
                        ]
              )
            ]

        payload =
            JE.object
                [ ( "TransactItems"
                  , JE.list JE.object <| List.map encodeGet getItems
                  )
                ]
    in
    makeFullRequest "TransactGetItems" payload (decoder getItems)


{-| Return the value of an item's string attribute.

If the attribute does not exist or doesn't have a string value, return `""`.

-}
itemStringValue : String -> Item -> String
itemStringValue key item =
    case Dict.get key item of
        Nothing ->
            ""

        Just attribute ->
            case attribute of
                StringValue s ->
                    s

                _ ->
                    ""


{-| Return the value of an item's float attribute.

If the attribute does not exist or doesn't have a float value, return `NaN`.

-}
itemFloatValue : String -> Item -> Float
itemFloatValue key item =
    case Dict.get key item of
        Nothing ->
            0 / 0

        Just attribute ->
            case attribute of
                NumberValue s ->
                    s

                _ ->
                    0 / 0


{-| Round an item's float value to an integer.

If the attribute does not exist or doesn't have a float value, returns
`NaN` as an integer, which can only be detected via `isNan (toFloat n)`.

-}
itemIntValue : String -> Item -> Int
itemIntValue string item =
    round <| itemFloatValue string item


{-| Remove the key fields from an `Item`.

The `Item` that comes back from `getItem` or `getItemWithMetadata` contains the key fields. Sometimes you'd rather not see them there.

-}
removeKeyFields : Key -> Item -> Item
removeKeyFields key item =
    case key of
        SimpleKey ( name, _ ) ->
            Dict.remove name item

        CompositeKey ( name1, _ ) ( name2, _ ) ->
            Dict.remove name1 item
                |> Dict.remove name2


{-| Return a list of the key names in a `Key`.
-}
keyNames : Key -> List String
keyNames key =
    case key of
        SimpleKey ( name, _ ) ->
            [ name ]

        CompositeKey ( name1, _ ) ( name2, _ ) ->
            [ name1, name2 ]


{-| Create a `Task` to send a signed request over the wire.
-}
send : Account -> Request a -> Task Error a
send account request =
    let
        service =
            makeService account

        credentials =
            makeCredentials account

        req2 =
            addHeaders [ AnyQuery "Accept" "*/*" ] request
    in
    AWS.Http.send service credentials req2
        |> Task.onError
            (\error ->
                (case error of
                    AWS.Http.HttpError err ->
                        HttpError err

                    AWS.Http.AWSError err ->
                        AWSError err
                )
                    |> Task.fail
            )


formatQuery : Query -> List ( String, String )
formatQuery query =
    let
        formatElement =
            \element ->
                case element of
                    AnyQuery k v ->
                        ( k, v )

                    Delimiter s ->
                        ( "delimiter", s )

                    Marker s ->
                        ( "marker", s )

                    MaxKeys cnt ->
                        ( "max-keys", String.fromInt cnt )

                    Prefix s ->
                        ( "prefix", s )
    in
    List.map formatElement query


{-| Add headers to a `Request`.
-}
addHeaders : Query -> Request a -> Request a
addHeaders headers req =
    AWS.Http.addHeaders (formatQuery headers) req


{-| Add query parameters to a `Request`.
-}
addQuery : Query -> Request a -> Request a
addQuery query req =
    AWS.Http.addQuery (formatQuery query) req


{-| Low-level request creator.

Similar to`AWS.Http.request`, but assumes the HTTP result is JSON and
uses `AWS.Http.awsAppErrDecoder` as the `ErrorDecoder`.

-}
makeRequest : String -> Value -> Decoder a -> Request a
makeRequest name value decoder =
    AWS.Http.request name
        POST
        "/"
        (AWS.Http.jsonBody value)
        (AWS.Http.jsonBodyDecoder decoder)
        AWS.Http.awsAppErrDecoder


{-| Request creator with retained `Metadata`

Same as `makeRequest`, but the decoder takes metadata

-}
makeFullRequest : String -> Value -> (Metadata -> Decoder a) -> Request a
makeFullRequest name value decoder =
    AWS.Http.request name
        POST
        "/"
        (AWS.Http.jsonBody value)
        (AWS.Http.jsonFullDecoder decoder)
        AWS.Http.awsAppErrDecoder
