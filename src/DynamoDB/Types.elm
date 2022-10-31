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
    , Query, QueryElement(..)
    )

{-| Types for DynamoDB module


# Types

@docs Error, Account
@docs Query, QueryElement

-}

import AWS.Http
import AWS.Service as Service exposing (Service)
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
