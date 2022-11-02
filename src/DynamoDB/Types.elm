----------------------------------------------------------------------
--
-- DynamoDB/Types.elm
-- Types for elm-dynamodb
-- Copyright (c) 2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module DynamoDB.Types exposing
    ( Error(..), Account
    , Key(..), Item, AttributeValue(..)
    , Query, QueryElement(..)
    )

{-| Types for DynamoDB module


# Types

@docs Error, Account
@docs Key, Item, AttributeValue
@docs Query, QueryElement

-}

import AWS.Http
import AWS.Service as Service exposing (Service)
import Dict exposing (Dict)
import Http


{-| Errors returned from S3 operations

`HttpError` is from the standard Elm `Http` module.

`AWSError` is from the AWS.Http module.

`DecodeError` denotes a Decoder error in parsing DynamoDB account info.

-}
type Error
    = HttpError Http.Error
    | AWSError AWS.Http.AWSAppError
    | DecodeError String


{-| Information about a DynamoDB account
-}
type alias Account =
    { name : String
    , region : Maybe String
    , accessKey : String
    , secretKey : String
    , table : TableName
    }


{-| The name of a DynamoDB table.
-}
type alias TableName =
    String


{-| An HTTP mimetype, e.g. "text/html".
-}
type alias Mimetype =
    String


{-| A value in a database key or item.

See: <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_AttributeValue.html>

-}
type AttributeValue
    = BinaryValue String -- B
    | BoolValue Bool -- BOOL
    | BinarySetValue (List String) -- BS
    | ListValue (List AttributeValue) -- L
    | MapValue (Dict String AttributeValue) -- M
    | NumberValue Float -- N
    | NumberSetValue (List Float) -- NS
    | NullValue -- NULL
    | StringValue String -- S
    | StringSetValue (List String) -- SS


{-| A key for a database item.
-}
type Key
    = SimpleKey ( String, AttributeValue )
    | CompositeKey ( String, AttributeValue ) ( String, AttributeValue )


{-| An item in a database.
-}
type alias Item =
    Dict String AttributeValue


{-| An element of a `Query`, used for HTTP headers and query parameters.

`AnyQuery` allows you to encode any key/value pair.

The others are used as query parameters with `S3.listKeys`.

-}
type QueryElement
    = AnyQuery String String
    | Delimiter String
    | Marker String
    | MaxKeys Int
    | Prefix String


{-| A list of `QueryElement`s.
-}
type alias Query =
    List QueryElement
