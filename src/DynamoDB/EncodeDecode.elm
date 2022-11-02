----------------------------------------------------------------------
--
-- DynamoDB/EncodeDecode.elm
-- Types for elm-dynamodb
-- Copyright (c) 2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module DynamoDB.EncodeDecode exposing
    ( encodeKey, keyDecoder
    , encodeItem, itemDecoder
    , encodeAttributeValue, attributeValueDecoder
    )

{-| Encoders and decoders

@docs encodeKey, keyDecoder
@docs encodeItem, itemDecoder
@docs encodeAttributeValue, attributeValueDecoder

-}

import Base64.Decode
import Base64.Encode
import DynamoDB.Types exposing (AttributeValue(..), Item, Key(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Encode a `Key`
-}
encodeKey : Key -> Value
encodeKey key =
    case key of
        SimpleKey ( name, value ) ->
            JE.object [ ( name, encodeAttributeValue value ) ]

        -- Can't really call these "partition" and "sort" keys, since
        -- they can be in either order.
        CompositeKey ( name1, value1 ) ( name2, value2 ) ->
            JE.object
                [ ( name1, encodeAttributeValue value2 )
                , ( name2, encodeAttributeValue value2 )
                ]


{-| Decoder for a `Key`
-}
keyDecoder : Decoder Key
keyDecoder =
    JD.keyValuePairs attributeValueDecoder
        |> JD.andThen
            (\pairs ->
                case pairs of
                    [ pair ] ->
                        JD.succeed <| SimpleKey pair

                    [ pair1, pair2 ] ->
                        JD.succeed <| CompositeKey pair1 pair2

                    _ ->
                        JD.fail "Other than 1 or 2 key/value pairs."
            )


{-| Encode an `Item`.
-}
encodeItem : Item -> Value
encodeItem item =
    JE.dict identity encodeAttributeValue item


{-| Decoder for an `Item`
-}
itemDecoder : Decoder Item
itemDecoder =
    JD.dict attributeValueDecoder


stringToBase64 : String -> String
stringToBase64 string =
    Base64.Encode.encode <| Base64.Encode.string string


base64ToString : String -> Result String String
base64ToString string =
    case Base64.Decode.decode Base64.Decode.string string of
        Ok res ->
            Ok res

        Err err ->
            case err of
                Base64.Decode.ValidationError ->
                    Err "Base64 validation error."

                Base64.Decode.InvalidByteSequence ->
                    Err "Base64 invalid byte sequence."


{-| Encode an `AttributeValue`
-}
encodeAttributeValue : AttributeValue -> Value
encodeAttributeValue value =
    case value of
        BinaryValue string ->
            JE.object [ ( "B", JE.string <| stringToBase64 string ) ]

        BoolValue bool ->
            JE.object [ ( "BOOL", JE.bool bool ) ]

        BinarySetValue strings ->
            JE.object [ ( "BS", JE.list encodeBase64 strings ) ]

        ListValue values ->
            JE.object [ ( "L", JE.list encodeAttributeValue values ) ]

        MapValue dict ->
            JE.object [ ( "M", JE.dict identity encodeAttributeValue dict ) ]

        NumberValue float ->
            JE.object [ ( "N", JE.string <| String.fromFloat float ) ]

        NumberSetValue floats ->
            JE.object [ ( "NS", JE.list JE.string (List.map String.fromFloat floats) ) ]

        NullValue ->
            JE.object [ ( "NULL", JE.bool True ) ]

        StringValue string ->
            JE.object [ ( "S", JE.string string ) ]

        StringSetValue strings ->
            JE.object [ ( "SS", JE.list JE.string strings ) ]


encodeBase64 : String -> Value
encodeBase64 string =
    JE.string <| stringToBase64 string


base64Decoder : Decoder String
base64Decoder =
    JD.string
        |> JD.andThen
            (\string ->
                case base64ToString string of
                    Ok s ->
                        JD.succeed s

                    Err s ->
                        JD.fail s
            )


applyDecoder : (a -> AttributeValue) -> Decoder a -> Value -> Decoder AttributeValue
applyDecoder tag decoder value =
    case JD.decodeValue decoder value of
        Ok av ->
            JD.succeed <| tag av

        Err err ->
            JD.fail <| JD.errorToString err


floatStringDecoder : Decoder Float
floatStringDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case String.toFloat s of
                    Just f ->
                        JD.succeed f

                    Nothing ->
                        JD.fail <| "malformed float: " ++ s
            )


{-| Decoder for an `AttributeValue`
-}
attributeValueDecoder : Decoder AttributeValue
attributeValueDecoder =
    JD.keyValuePairs JD.value
        |> JD.andThen
            (\pairs ->
                case pairs of
                    [ ( key, value ) ] ->
                        case key of
                            "B" ->
                                applyDecoder BinaryValue base64Decoder value

                            "BOOL" ->
                                applyDecoder BoolValue JD.bool value

                            "BS" ->
                                applyDecoder BinarySetValue
                                    (JD.list base64Decoder)
                                    value

                            "L" ->
                                applyDecoder ListValue
                                    (JD.list attributeValueDecoder)
                                    value

                            "M" ->
                                applyDecoder MapValue
                                    (JD.dict attributeValueDecoder)
                                    value

                            "N" ->
                                applyDecoder NumberValue floatStringDecoder value

                            "NS" ->
                                applyDecoder NumberSetValue
                                    (JD.list floatStringDecoder)
                                    value

                            "NULL" ->
                                JD.succeed NullValue

                            "S" ->
                                applyDecoder StringValue JD.string value

                            "SS" ->
                                applyDecoder StringSetValue (JD.list JD.string) value

                            _ ->
                                JD.fail <| "Unknown value type: " ++ key

                    _ ->
                        JD.fail "Not exactly one value."
            )
