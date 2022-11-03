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
import DynamoDB as DynamoDB
import DynamoDB.EncodeDecode as ED
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
    , metadata : Maybe Metadata
    }


type Msg
    = SetAccount String
    | SetKeyName String
    | SetKeyValue String
    | SetText String
    | GetItem
    | ReceiveGetItem Key (Result Error ( Metadata, Maybe Item ))
    | ReceiveAccounts (Result Error (List Account))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , key = SimpleKey ( "key", StringValue "test" )
      , item = Nothing
      , text = ""
      , metadata = Nothing
      }
    , Task.attempt ReceiveAccounts (DynamoDB.readAccounts Nothing)
    )


getItem : Model -> ( Model, Cmd Msg )
getItem model =
    ( { model
        | item = Nothing
        , text = ""
        , metadata = Nothing
      }
    , DynamoDB.getFullItem model.account.tableName model.key
        |> DynamoDB.send model.account
        |> Task.attempt (ReceiveGetItem model.key)
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

        GetItem ->
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
                getItem
                    { model
                        | display =
                            "Fetching " ++ keyToString model.key ++ "..."
                    }

        ReceiveGetItem key result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok ( metadata, res ) ->
                    let
                        mdl =
                            { model | metadata = Just metadata }
                    in
                    ( case res of
                        Just item ->
                            let
                                i =
                                    Debug.log "item" item

                                item2 =
                                    DynamoDB.removeKeyFields key item
                            in
                            { mdl
                                | display = "Got: " ++ keyToString key
                                , item = Just item2
                                , text = ED.encodeItem item2 |> JE.encode 2
                            }

                        Nothing ->
                            { mdl
                                | display = "No value for: " ++ keyToString key
                                , item = Nothing
                                , text = ""
                            }
                    , Cmd.none
                    )

        SetText text ->
            ( { model | text = text }
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
                        , display = "Accounts received."
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
        [ p [] [ text model.display ]
        , p []
            [ text "Account: "
            , accountSelector model
            ]
        , p []
            [ text "Table Name: "
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
            , br
            , button [ onClick GetItem ]
                [ text "GetItem" ]
            ]

        {-
           , p []
               [ button [ onClick PutItem ]
                   [ text "PutItem" ]
               ]
        -}
        , p []
            [ textarea
                [ cols 80
                , rows 20
                , value model.text
                , onInput SetText
                ]
                []
            ]
        , case model.item of
            Nothing ->
                text ""

            Just item ->
                p []
                    [ text "Item: "
                    , text <| Debug.toString (Dict.toList item)
                    ]
        , case model.metadata of
            Nothing ->
                text ""

            Just metadata ->
                p []
                    [ text "Headers: "
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
