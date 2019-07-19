module Types exposing
    ( DataSchema
    , Introspected
    , SchemaType
    , TypeField
    , decodeIntrospected
    , introspectedDict
    , queries
    )

import Dict exposing (Dict)
import Json.Decode


type alias Introspected =
    { data : IntrospectedData }


type alias IntrospectedData =
    { schema : DataSchema }


type alias DataSchema =
    { mutationType : Maybe NamedType
    , queryType : Maybe NamedType
    , types : List SchemaType
    }


type alias NamedType =
    { name : String
    }


type alias SchemaType =
    { kind : KindType
    , name : String
    , description : Maybe String
    , fields : Maybe (List TypeField)
    }


type KindType
    = SCHEMA
    | SCALAR
    | OBJECT
    | FIELD_DEFINITION
    | ARGUMENT_DEFINITION
    | INTERFACE
    | UNION
    | ENUM
    | ENUM_VALUE
    | INPUT_OBJECT
    | INPUT_FIELD_DEFINITION


decodeKindType : Json.Decode.Decoder KindType
decodeKindType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "SCHEMA" ->
                        Json.Decode.succeed SCHEMA

                    "SCALAR" ->
                        Json.Decode.succeed SCALAR

                    "OBJECT" ->
                        Json.Decode.succeed OBJECT

                    "FIELD_DEFINITION" ->
                        Json.Decode.succeed FIELD_DEFINITION

                    "ARGUMENT_DEFINITION" ->
                        Json.Decode.succeed ARGUMENT_DEFINITION

                    "INTERFACE" ->
                        Json.Decode.succeed INTERFACE

                    "UNION" ->
                        Json.Decode.succeed UNION

                    "ENUM" ->
                        Json.Decode.succeed ENUM

                    "ENUM_VALUE" ->
                        Json.Decode.succeed ENUM_VALUE

                    "INPUT_OBJECT" ->
                        Json.Decode.succeed INPUT_OBJECT

                    "INPUT_FIELD_DEFINITION" ->
                        Json.Decode.succeed INPUT_FIELD_DEFINITION

                    _ ->
                        Json.Decode.fail "Invalid KindType"
            )


type alias TypeField =
    { name : Maybe String
    , args : Maybe (List NamedType)
    , type_ : Maybe NamedType
    , description : Maybe String
    }


decodeTypeField : Json.Decode.Decoder TypeField
decodeTypeField =
    Json.Decode.map4 TypeField
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "args" (Json.Decode.list decodeNamedType)))
        (Json.Decode.maybe (Json.Decode.field "type" decodeNamedType))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))


introspectedDict : DataSchema -> Dict String SchemaType
introspectedDict schema =
    List.foldl (\t sum -> Dict.insert t.name t sum) Dict.empty schema.types


queries : Dict String SchemaType -> DataSchema -> List ( TypeField, SchemaType )
queries typeDict schema =
    case schema.queryType of
        Just namedType ->
            case Dict.get namedType.name typeDict of
                Just foundType ->
                    List.foldl
                        (\f sum ->
                            case f.type_ of
                                Nothing ->
                                    sum

                                Just ft ->
                                    case Dict.get ft.name typeDict of
                                        Nothing ->
                                            sum

                                        Just t ->
                                            ( f, t ) :: sum
                        )
                        []
                        (Maybe.withDefault [] foundType.fields)

                Nothing ->
                    []

        Nothing ->
            []


decodeNamedType : Json.Decode.Decoder NamedType
decodeNamedType =
    Json.Decode.map2
        (\maybeName maybeNamedType ->
            Maybe.withDefault { name = Maybe.withDefault "noname" maybeName } maybeNamedType
        )
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "ofType" (Json.Decode.map NamedType (Json.Decode.field "name" Json.Decode.string))))


decodeIntrospected : Json.Decode.Decoder Introspected
decodeIntrospected =
    Json.Decode.map Introspected
        (Json.Decode.field "data"
            (Json.Decode.map IntrospectedData
                (Json.Decode.field "__schema"
                    (Json.Decode.map3 DataSchema
                        (Json.Decode.maybe (Json.Decode.field "mutationType" decodeNamedType))
                        (Json.Decode.maybe (Json.Decode.field "queryType" decodeNamedType))
                        (Json.Decode.field "types"
                            (Json.Decode.list
                                (Json.Decode.map4 SchemaType
                                    (Json.Decode.field "kind" decodeKindType)
                                    (Json.Decode.field "name" Json.Decode.string)
                                    (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
                                    (Json.Decode.maybe (Json.Decode.field "fields" (Json.Decode.list decodeTypeField)))
                                )
                            )
                        )
                    )
                )
            )
        )
