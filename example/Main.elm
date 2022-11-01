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
import DynamoDB exposing (readAccounts)
import DynamoDB.Types
    exposing
        ( Account
        , Error(..)
        , QueryElement(..)
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
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
import Json.Decode as JD
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
    , table : String
    , key : String
    , secondaryKey : String
    , text : String
    , mimetype : String
    , headers : List ( String, String )
    }


type Msg
    = SetAccount String
    | ReceiveAccounts (Result Error (List Account))
    | ReceiveGetObject (Result Error ( String, Dict String String ))
    | SetTable String
    | ListBucket
    | SetKey String
    | SetSecondaryKey String
    | GetItem
    | GetKey String
    | SetText String
    | SetMimetype String
    | PutObject
    | ReceivePutObject (Result Error String)
    | DeleteObject
    | ReceiveDeleteObject (Result Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , table = "no table"
      , key = ""
      , secondaryKey = ""
      , text = ""
      , mimetype = "plain"
      , headers = []
      }
    , Task.attempt ReceiveAccounts (readAccounts Nothing)
    )


getItem : Model -> Cmd Msg
getItem model =
    S3.getObjectWithHeaders model.bucket model.key
        |> S3.send model.account
        |> Task.attempt ReceiveGetObject


putObject : Model -> Cmd Msg
putObject model =
    let
        body =
            S3.stringBody ("text/" ++ model.mimetype ++ "; charset=utf-8") model.text
    in
    S3.putPublicObject model.bucket model.key body
        |> S3.send model.account
        |> Task.attempt ReceivePutObject


deleteObject : Model -> Cmd Msg
deleteObject model =
    S3.deleteObject model.bucket model.key
        |> S3.send model.account
        |> Task.attempt ReceiveDeleteObject


defaultAccount : Account
defaultAccount =
    { name = "No account"
    , region = Nothing
    , accessKey = ""
    , secretKey = ""
    , buckets = [ "No bucket" ]
    , isDigitalOcean = False
    }


findAccount : Model -> String -> Account
findAccount model name =
    case LE.find (\a -> a.name == name) model.accounts of
        Nothing ->
            defaultAccount

        Just a ->
            a


stringEqual : String -> String -> Bool
stringEqual s1 s2 =
    String.toLower s1 == String.toLower s2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAccount name ->
            let
                account =
                    findAccount model name

                bucket =
                    case account.buckets of
                        b :: _ ->
                            b

                        _ ->
                            "No bucket"
            in
            ( { model
                | account = account
                , bucket = bucket
                , display = "Account: " ++ name
              }
            , Cmd.none
            )

        SetBucket bucket ->
            ( { model | bucket = bucket }
            , Cmd.none
            )

        ListBucket ->
            ( { model | display = "Getting bucket listing..." }
            , listBucket model
            )

        SetKey key ->
            ( { model | key = key }
            , Cmd.none
            )

        GetObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )

            else
                ( { model | display = "Fetching " ++ model.key ++ "..." }
                , getObject model
                )

        ReceiveGetObject result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok ( res, headers ) ->
                    ( { model
                        | display = "Got " ++ model.key
                        , text = res
                        , headers = Dict.toList headers
                      }
                    , Cmd.none
                    )

        GetKey key ->
            ( { model | key = key }
            , Task.perform (\_ -> GetObject) <| Task.succeed ()
            )

        SetText text ->
            ( { model | text = text }
            , Cmd.none
            )

        SetMimetype mimetype ->
            ( { model | mimetype = mimetype }
            , Cmd.none
            )

        PutObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )

            else
                ( { model | display = "Writing " ++ model.key ++ "..." }
                , putObject model
                )

        ReceivePutObject result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok res ->
                    ( { model | display = "Put " ++ model.key }
                    , Cmd.none
                    )

        DeleteObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )

            else
                ( { model | display = "Deleting " ++ model.key ++ "..." }
                , deleteObject model
                )

        ReceiveDeleteObject result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok res ->
                    ( { model | display = "Deleted " ++ model.key }
                    , Cmd.none
                    )

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
                        , bucket =
                            case account.buckets of
                                b :: _ ->
                                    b

                                _ ->
                                    "No bucket"
                        , display = "Accounts received."
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        [ style "margin-left" "3em"
        ]
        [ p [] [ text model.display ]
        , p []
            [ text "Account: "
            , accountSelector model
            ]
        , p []
            [ text "Bucket: "
            , bucketSelector model
            , text " "
            , button [ onClick ListBucket ]
                [ text "List Bucket" ]
            ]
        , p []
            [ text "Key: "
            , input
                [ type_ "text"
                , size 40
                , value model.key
                , onInput SetKey
                ]
                []
            , text " "
            , button [ onClick GetObject ]
                [ text "Get" ]
            , text " "
            , button [ onClick DeleteObject ]
                [ text "Delete" ]
            ]
        , p []
            [ text "URL: "
            , let
                request =
                    S3.getObject model.bucket model.key

                url =
                    S3.requestUrl model.account request
              in
              text url
            ]
        , p []
            [ input
                [ type_ "radio"
                , name "mimetype"
                , onClick (SetMimetype "plain")
                , checked <| model.mimetype == "plain"
                ]
                []
            , text " plain "
            , input
                [ type_ "radio"
                , name "mimetype"
                , onClick (SetMimetype "html")
                , checked <| model.mimetype == "html"
                ]
                []
            , text " html "
            , button [ onClick PutObject ]
                [ text "Put" ]
            ]
        , p []
            [ textarea
                [ cols 80
                , rows 20
                , value model.text
                , onInput SetText
                ]
                []
            ]
        , p []
            [ text "Headers: "
            , text <| Debug.toString model.headers
            ]
        ]


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


bucketSelector : Model -> Html Msg
bucketSelector model =
    select [ on "change" (JD.map SetBucket targetValue) ]
        (List.map (bucketOption model) model.account.buckets)


bucketOption : Model -> String -> Html Msg
bucketOption model bucket =
    option
        [ value bucket
        , selected (model.bucket == bucket)
        ]
        [ text bucket ]


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
