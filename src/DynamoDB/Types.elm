----------------------------------------------------------------------
--
-- DynamoDB/Types.elm
-- Types for elm-dynamodb
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module DynamoDB.Types exposing
    ( TableName
    , Key(..), Item, AttributeValue(..)
    , Error(..), Account
    , emptyAccount
    )

{-| Types for DynamoDB module


# Types

@docs TableName
@docs Key, Item, AttributeValue
@docs Error, Account


# Utilities

@docs emptyAccount

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
    , tableName : TableName
    }


{-| An empty `DynamoDB.Account`.

This belongs in the DynamoDB module. Move when adding this to that.

-}
emptyAccount : Account
emptyAccount =
    { name = "<empty>"
    , region = Nothing
    , accessKey = ""
    , secretKey = ""
    , tableName = ""
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
