----------------------------------------------------------------------
--
-- DynamoDB/Html.elm
-- Html widgets for elm-dynamodb
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module DynamoDB.Html exposing (itemTable)

{-| A few HTML widgets to ease in displaying DynamoDB data.
-}

import Dict exposing (Dict)
import DynamoDB.Types exposing (AttributeValue(..), Item, Key(..), TableName)
import Html
    exposing
        ( Attribute
        , Html
        , div
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( class
        , value
        )
import Html.Events exposing (onClick)
import Set exposing (Set)


{-| The HTML `style` element.

It's not supposed to work to put this in the `body` of a document, but it does.

-}
styleElement : List (Attribute msg) -> List (Html msg) -> Html msg
styleElement =
    Html.node "style"


prettytableStyle : String
prettytableStyle =
    """
table.prettytable {
  margin: 0em 0.5em 0.5em 0.5em;
  background: whitesmoke;
  border-collapse: collapse;
}
table.prettytable th, table.prettytable td {
  border: 1px silver solid;
  padding: 0.2em;
}
table.prettytable th {
  background: gainsboro;
  text-align: center;
}
table.prettytable tr:nth-child(even) td {
  background: white;
}
table.prettytable caption {
  margin-left: inherit;
  margin-right: inherit;
}"""


itemsColumnNames : List Item -> List String
itemsColumnNames items =
    let
        outer item set =
            let
                inner columnName s =
                    Set.insert columnName s
            in
            List.foldr inner set <| Dict.keys item
    in
    List.foldl outer Set.empty items
        |> Set.toList


{-| Render a table for a list of `Item`s.

The wrapper (`(Item -> msg)`) is called when the user clicks on a row.

-}
itemTable : (Item -> msg) -> List Item -> Html msg
itemTable wrapper items =
    let
        columnNames =
            itemsColumnNames items
    in
    div []
        [ styleElement []
            [ text prettytableStyle ]
        , table
            [ class "prettytable" ]
            ([ tr [] <|
                List.map (\name -> th [] [ text name ]) columnNames
             ]
                ++ List.map (itemRow wrapper columnNames) items
            )
        ]


itemRow : (Item -> msg) -> List String -> Item -> Html msg
itemRow wrapper columnNames item =
    let
        itemTd columnName =
            let
                str =
                    case Dict.get columnName item of
                        Nothing ->
                            ""

                        Just value ->
                            attributeValueToText value
            in
            td [] [ text str ]
    in
    tr [ onClick <| wrapper item ] <|
        List.map itemTd columnNames


attributeValueToText : AttributeValue -> String
attributeValueToText value =
    case value of
        NullValue ->
            "NULL"

        StringValue s ->
            "\"" ++ s ++ "\""

        NumberValue f ->
            String.fromFloat f

        -- TODO: flesh out more of these
        _ ->
            Debug.toString value
