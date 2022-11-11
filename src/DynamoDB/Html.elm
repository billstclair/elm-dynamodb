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


module DynamoDB.Html exposing
    ( renderItemTable
    , TableConfig, renderTable
    )

{-| Simple rendering for a list of `DynamoDB.Types.Item`.

@docs renderItemTable
@docs TableConfig, renderTable

-}

import Dict exposing (Dict)
import DynamoDB.Types exposing (AttributeValue(..), Item, TableName)
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


itemsColumnNames : List String -> List Item -> List String
itemsColumnNames keyNames items =
    let
        outer item set =
            let
                inner columnName s =
                    Set.insert columnName s
            in
            List.foldr inner set <| Dict.keys item

        deleter keyName list =
            List.filter (\x -> x /= keyName) list
    in
    List.foldl outer Set.empty items
        |> Set.toList
        |> (\list ->
                List.concat
                    [ keyNames
                    , List.foldr deleter list keyNames
                    ]
           )


{-| Render a table for a list of `Item`s.

The list of strings is the key names which should appear first in the
displayed table.

The wrapper, `(Item -> msg)`, is called when the user clicks on a row.

-}
renderItemTable : (Item -> msg) -> List String -> List Item -> Html msg
renderItemTable wrapper keyNames items =
    let
        config =
            { itemTableConfig
                | columnDescriptors = itemsColumnNames keyNames items
            }
    in
    renderTable wrapper config items


itemTableConfig =
    { columnDescriptors = []
    , columnDescriptorToString = identity
    , elementToString = itemToString
    }


itemToString : String -> Item -> Maybe String
itemToString columnName item =
    case Dict.get columnName item of
        Nothing ->
            Nothing

        Just value ->
            Just <| attributeValueToText value


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


{-| Describe a table column for `renderTable`.
-}
type alias TableConfig element columnDescriptor =
    { columnDescriptors : List columnDescriptor
    , columnDescriptorToString : columnDescriptor -> String
    , elementToString : columnDescriptor -> element -> Maybe String
    }


{-| Render a table. `renderItemTable` is implemented with this.

The wrapper, `(item -> msg)`, is called when the user clicks on a row.

-}
renderTable : (element -> msg) -> TableConfig element columnDescriptor -> List element -> Html msg
renderTable wrapper config elements =
    let
        columnNames =
            List.map config.columnDescriptorToString config.columnDescriptors
    in
    div []
        [ styleElement []
            [ text prettytableStyle ]
        , table
            [ class "prettytable" ]
            ([ tr [] <|
                List.map (\name -> th [] [ text name ]) columnNames
             ]
                ++ List.map (elementRow wrapper config) elements
            )
        ]


elementRow : (element -> msg) -> TableConfig element columnDescriptor -> element -> Html msg
elementRow wrapper config element =
    let
        elementTd columnDescriptor =
            let
                str =
                    Maybe.withDefault "" <|
                        config.elementToString columnDescriptor element
            in
            td [] [ text str ]
    in
    tr [ onClick <| wrapper element ] <|
        List.map elementTd config.columnDescriptors
