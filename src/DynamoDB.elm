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
    , send
    , readAccounts, decodeAccounts, accountDecoder, encodeAccount
    )

{-| Pure Elm client for the [AWS DynamoDB](https://aws.amazon.com/dynamodb/) NoSQL database service.


# Types

@docs Request


# Turning a Request into a Task

@docs send


# Creating DynamoDB requests

@docs getItem, putItem


# Creating Body values


# Adding queries and headers to a request


# Reading accounts into Elm

@docs readAccounts, decodeAccounts, accountDecoder, encodeAccount


# Low-level functions

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
        , request
        )
import AWS.Service as Service exposing (Service)
import Dict exposing (Dict)
import DynamoDB.Types as Types exposing (Account, Error(..), Query, QueryElement(..))
import Http exposing (Metadata)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Task exposing (Task)


defaultAccountsUrl : String
defaultAccountsUrl =
    "accounts.json"


{-| Read JSON from a URL and turn it into a list of `Account`s.

If `Nothing` is passed for the first arg (the URL), will use the default of `"accounts.json"`.

You're not going to want to store the secret keys in this JSON in plain text anywhere but your development machine. In applications, they will ususally be stored in `LocalStorage`.

Example JSON (the `table` is used only by the example code):

    [{"name": "Dynamo DB",
      "region": "us-east-1",
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "table": "<your table name>"
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
              , ( "table", JE.string account.table )
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
        (JD.field "table" JD.string)


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


awsEndpointPrefix : String
awsEndpointPrefix =
    "dynamodb"


apiVersion : Config.ApiVersion
apiVersion =
    "20120810"


protocol : Config.Protocol
protocol =
    Config.REST_XML


{-| Make an `AWS.Service.Service` for a `DynamoDB.Account`.

Sometimes useful for the `hostResolver`.

-}
makeService : Account -> Service
makeService { region } =
    let
        prefix =
            -- Changed by `send` to the bucket for Digital Ocean.
            awsEndpointPrefix

        service =
            case region of
                Nothing ->
                    Service.service <|
                        Config.defineGlobal
                            prefix
                            apiVersion
                            protocol
                            Config.SignV4

                Just reg ->
                    Service.service <|
                        Config.defineRegional
                            prefix
                            apiVersion
                            protocol
                            Config.SignV4
                            reg
    in
    service


{-| A request that can be turned into a Task by `S3.send`.

`a` is the type of the successful `Task` result from `S3.send`.

-}
type alias Request a =
    AWS.Http.Request AWSAppError a


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

    stringRequest : String -> Method -> Path -> Body -> Request String
    stringRequest method url body =
        parserRequest
            name
            method
            url
            body
            (identity >> Ok)
            Task.succeed

-}
parserRequest : String -> Method -> Path -> Body -> (String -> Result String a) -> Request a
parserRequest name method path body parser =
    request name
        method
        path
        body
        (AWS.Http.stringBodyDecoder parser)
        AWS.Http.awsAppErrDecoder


{-| Create a `Request` that returns its response body as a string.

    getObject : String -> Method -> Request String
    getObject bucket key =
        stringRequest "operation" GET (objectPath bucket key) emptyBody

-}
stringRequest : String -> Method -> Path -> Body -> Request String
stringRequest name method path body =
    parserRequest name method path body (identity >> Ok)



--- TODO: Requests


{-| Return the URL string for a request.
-}
requestUrl : Account -> Request a -> String
requestUrl account request =
    let
        { hostResolver, endpoint, endpointPrefix } =
            makeService account

        host =
            hostResolver endpoint endpointPrefix
    in
    "https://"
        ++ host
        ++ request.path


{-| Read an object and process the entire Http Response.
-}
getFullObject : String -> String -> (Metadata -> String -> Result String a) -> Request a
getFullObject bucket key parser =
    request "getFullObject"
        GET
        (objectPath bucket key)
        emptyBody
        parser
        AWS.Http.awsAppErrDecoder


{-| Turn a bucket and a key into an object path.

    "/" ++ bucket ++ "/" ++ key

-}
objectPath : String -> String -> String
objectPath bucket key =
    "/" ++ bucket ++ "/" ++ key


responseHeaders : Metadata -> String -> Result String ( String, Dict String String )
responseHeaders metadata body =
    Ok <| ( body, metadata.headers )


{-| Read an object with its HTTP response headers.
-}
getObjectWithHeaders : String -> String -> Request ( String, Dict String String )
getObjectWithHeaders bucket key =
    getFullObject bucket
        key
        responseHeaders


responseHeadersOnly : Metadata -> String -> Result String (Dict String String)
responseHeadersOnly metadata body =
    case responseHeaders metadata body of
        Ok ( _, headers ) ->
            Ok headers

        Err e ->
            Err e


{-| Do a HEAD request to get only an object's headers.
-}
getHeaders : String -> String -> Request (Dict String String)
getHeaders bucket key =
    getFullObject bucket
        key
        responseHeadersOnly
