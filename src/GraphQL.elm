module GraphQL exposing
    ( Field
    , InputValue
    , Response
    , Schema
    , Type(..)
    , decodeIntrospectResponse
    , fieldType
    , fields
    , introspectionQuery
    , ofType
    , typeName
    , typesDict
    )

import Dict exposing (Dict)
import Json.Decode


type alias Response =
    { data : Data }


type alias Data =
    { schema : Schema }


decodeIntrospectResponse : Json.Decode.Decoder Response
decodeIntrospectResponse =
    Json.Decode.map Response
        (Json.Decode.field "data"
            (Json.Decode.map Data
                (Json.Decode.field "__schema" decodeSchema)
            )
        )


typeName : Type -> Maybe String
typeName type_ =
    case type_ of
        TypeScalar attrs ->
            attrs.name

        TypeObject attrs ->
            attrs.name

        TypeInterface attrs ->
            attrs.name

        TypeUnion attrs ->
            attrs.name

        TypeEnum attrs ->
            attrs.name

        TypeInput attrs ->
            attrs.name

        TypeNotNull attrs ->
            typeName attrs.ofType

        TypeList attrs ->
            typeName attrs.ofType


typeFields : Type -> List Field
typeFields type_ =
    case type_ of
        TypeScalar attrs ->
            []

        TypeObject attrs ->
            Maybe.withDefault [] attrs.fields

        TypeInterface attrs ->
            Maybe.withDefault [] attrs.fields

        TypeUnion attrs ->
            []

        TypeEnum attrs ->
            []

        TypeInput attrs ->
            []

        TypeNotNull attrs ->
            []

        TypeList attrs ->
            []


typesDict : List Type -> Dict String Type -> Dict String Type
typesDict typeList sum =
    case typeList of
        [] ->
            sum

        x :: xs ->
            let
                name =
                    typeName x
            in
            case name of
                Nothing ->
                    typesDict xs sum

                Just s ->
                    typesDict xs (Dict.insert s x sum)


ofType : Dict String Type -> Type -> Type
ofType typeStringDict type_ =
    typeName type_
        |> Maybe.andThen (\s -> Dict.get s typeStringDict)
        |> Maybe.withDefault type_


fieldType : Dict String Type -> Field -> Maybe Type
fieldType typeStringDict field =
    typeName field.type_
        |> Maybe.andThen (\s -> Dict.get s typeStringDict)


fields : Dict String Type -> Type -> List Field
fields typeStringDict type_ =
    typeName type_
        |> Maybe.andThen (\s -> Dict.get s typeStringDict)
        |> Maybe.map typeFields
        |> Maybe.withDefault []



-- Type/Type aliases translated from
-- https://graphql.github.io/graphql-spec/June2018/#sec-Schema-Introspection


type alias Schema =
    { types : List Type
    , queryType : Type
    , mutationType : Maybe Type
    , subscriptionType : Maybe Type
    , directives : List Directive
    }


type Type
    = TypeScalar ScalarAttrs
    | TypeObject ObjectAttrs
    | TypeInterface InterfaceAttrs
    | TypeUnion UnionAttrs
    | TypeEnum EnumAttrs
    | TypeInput InputAttrs
    | TypeNotNull NotNullAttrs
    | TypeList ListAttrs


type alias ScalarAttrs =
    { name : Maybe String
    , description : Maybe String
    }


type alias ObjectAttrs =
    { name : Maybe String
    , description : Maybe String
    , fields : Maybe (List Field)
    , interfaces : Maybe (List Type)
    }


type alias InterfaceAttrs =
    { name : Maybe String
    , description : Maybe String
    , fields : Maybe (List Field)
    , possibleTypes : Maybe (List Type)
    }


type alias UnionAttrs =
    { name : Maybe String
    , description : Maybe String
    , possibleTypes : Maybe (List Type)
    }


type alias EnumAttrs =
    { name : Maybe String
    , description : Maybe String
    , enumValues : Maybe (List EnumValue)
    }


type alias InputAttrs =
    { name : Maybe String
    , description : Maybe String
    , inputFields : Maybe (List InputValue)
    }


type alias NotNullAttrs =
    { name : Maybe String
    , description : Maybe String
    , ofType : Type
    }


type alias ListAttrs =
    { name : Maybe String
    , description : Maybe String
    , ofType : Type
    }


type alias Field =
    { name : String
    , description : Maybe String
    , args : List InputValue
    , type_ : Type
    , isDeprecated : Bool
    , deprecationReason : Maybe String
    }


type alias InputValue =
    { name : String
    , description : Maybe String
    , type_ : Type
    , defaultValue : Maybe String
    }


type alias EnumValue =
    { name : String
    , description : Maybe String
    , isDeprecated : Bool
    , deprecationReason : Maybe String
    }


type TypeKind
    = KindSCALAR
    | KindOBJECT
    | KindINTERFACE
    | KindUNION
    | KindENUM
    | KindINPUT_OBJECT
    | KindLIST
    | KindNON_NULL


type alias Directive =
    { name : String
    , description : Maybe String
    , locations : List DirectiveLocation
    , args : List InputValue
    }


type DirectiveLocation
    = QUERY
    | MUTATION
    | SUBSCRIPTION
    | FIELD
    | FRAGMENT_DEFINITION
    | FRAGMENT_SPREAD
    | INLINE_FRAGMENT
    | SCHEMA
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


{-| Graphql query to obtain schema of endpoint
-}
introspectionQuery : String
introspectionQuery =
    """
    query IntrospectionQuery {
      __schema {
        queryType { kind name }
        mutationType { kind name }
        subscriptionType { kind name }
        types {
          ...FullType
        }
        directives {
          name
          description
          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
    """



-- Json Decoders (mostly generated by Elmjutsu autocomplete)


decodeSchema : Json.Decode.Decoder Schema
decodeSchema =
    Json.Decode.map5 Schema
        (Json.Decode.field "types" (Json.Decode.list decodeType))
        (Json.Decode.field "queryType" decodeType)
        (Json.Decode.maybe (Json.Decode.field "mutationType" decodeType))
        (Json.Decode.maybe (Json.Decode.field "subscriptionType" decodeType))
        (Json.Decode.field "directives"
            (Json.Decode.list
                (Json.Decode.map4 Directive
                    (Json.Decode.field "name" Json.Decode.string)
                    (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
                    (Json.Decode.field "locations" (Json.Decode.list decodeDirectiveLocation))
                    (Json.Decode.field "args" (Json.Decode.list decodeInputValue))
                )
            )
        )


decodeType : Json.Decode.Decoder Type
decodeType =
    Json.Decode.field "kind" decodeTypeKind
        |> Json.Decode.andThen
            (\kind ->
                case kind of
                    KindSCALAR ->
                        decodeScalarAttrs |> Json.Decode.map TypeScalar

                    KindOBJECT ->
                        decodeObjectAttrs |> Json.Decode.map TypeObject

                    KindINTERFACE ->
                        decodeInterfaceAttrs |> Json.Decode.map TypeInterface

                    KindUNION ->
                        decodeUnionAttrs |> Json.Decode.map TypeUnion

                    KindENUM ->
                        decodeEnumAttrs |> Json.Decode.map TypeEnum

                    KindINPUT_OBJECT ->
                        decodeInputAttrs |> Json.Decode.map TypeInput

                    KindLIST ->
                        decodeListAttrs |> Json.Decode.map TypeList

                    KindNON_NULL ->
                        decodeNotNullAttrs |> Json.Decode.map TypeNotNull
            )


decodeScalarAttrs : Json.Decode.Decoder ScalarAttrs
decodeScalarAttrs =
    Json.Decode.map2 ScalarAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))


decodeObjectAttrs : Json.Decode.Decoder ObjectAttrs
decodeObjectAttrs =
    Json.Decode.map4 ObjectAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "fields" (Json.Decode.list decodeField)))
        (Json.Decode.maybe (Json.Decode.field "interfaces" (Json.Decode.list decodeType)))


decodeInterfaceAttrs : Json.Decode.Decoder InterfaceAttrs
decodeInterfaceAttrs =
    Json.Decode.map4 InterfaceAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "fields" (Json.Decode.list decodeField)))
        (Json.Decode.maybe (Json.Decode.field "possibleTypes" (Json.Decode.list decodeType)))


decodeUnionAttrs : Json.Decode.Decoder UnionAttrs
decodeUnionAttrs =
    Json.Decode.map3 UnionAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "possibleTypes" (Json.Decode.list decodeType)))


decodeEnumAttrs : Json.Decode.Decoder EnumAttrs
decodeEnumAttrs =
    Json.Decode.map3 EnumAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.maybe
            (Json.Decode.field "enumValues"
                (Json.Decode.list decodeEnumValue)
            )
        )


decodeInputAttrs : Json.Decode.Decoder InputAttrs
decodeInputAttrs =
    Json.Decode.map3 InputAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "inputFields" (Json.Decode.list decodeInputValue)))


decodeNotNullAttrs : Json.Decode.Decoder NotNullAttrs
decodeNotNullAttrs =
    Json.Decode.map3 NotNullAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.field "ofType" decodeType)


decodeListAttrs : Json.Decode.Decoder ListAttrs
decodeListAttrs =
    Json.Decode.map3 ListAttrs
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.field "ofType" decodeType)


decodeField : Json.Decode.Decoder Field
decodeField =
    Json.Decode.map6 Field
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.field "args" (Json.Decode.list decodeInputValue))
        (Json.Decode.field "type" decodeType)
        (Json.Decode.field "isDeprecated" Json.Decode.bool)
        (Json.Decode.maybe (Json.Decode.field "deprecationReason" Json.Decode.string))


decodeInputValue : Json.Decode.Decoder InputValue
decodeInputValue =
    Json.Decode.map4 InputValue
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.field "type" decodeType)
        (Json.Decode.maybe (Json.Decode.field "defaultValue" Json.Decode.string))


decodeEnumValue : Json.Decode.Decoder EnumValue
decodeEnumValue =
    Json.Decode.map4 EnumValue
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.field "isDeprecated" Json.Decode.bool)
        (Json.Decode.maybe (Json.Decode.field "deprecationReason" Json.Decode.string))


decodeTypeKind : Json.Decode.Decoder TypeKind
decodeTypeKind =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "SCALAR" ->
                        Json.Decode.succeed KindSCALAR

                    "OBJECT" ->
                        Json.Decode.succeed KindOBJECT

                    "INTERFACE" ->
                        Json.Decode.succeed KindINTERFACE

                    "UNION" ->
                        Json.Decode.succeed KindUNION

                    "ENUM" ->
                        Json.Decode.succeed KindENUM

                    "INPUT_OBJECT" ->
                        Json.Decode.succeed KindINPUT_OBJECT

                    "LIST" ->
                        Json.Decode.succeed KindLIST

                    "NON_NULL" ->
                        Json.Decode.succeed KindNON_NULL

                    _ ->
                        Json.Decode.fail ("Invalid TypeKind: " ++ string)
            )


decodeDirectiveLocation : Json.Decode.Decoder DirectiveLocation
decodeDirectiveLocation =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "QUERY" ->
                        Json.Decode.succeed QUERY

                    "MUTATION" ->
                        Json.Decode.succeed MUTATION

                    "SUBSCRIPTION" ->
                        Json.Decode.succeed SUBSCRIPTION

                    "FIELD" ->
                        Json.Decode.succeed FIELD

                    "FRAGMENT_DEFINITION" ->
                        Json.Decode.succeed FRAGMENT_DEFINITION

                    "FRAGMENT_SPREAD" ->
                        Json.Decode.succeed FRAGMENT_SPREAD

                    "INLINE_FRAGMENT" ->
                        Json.Decode.succeed INLINE_FRAGMENT

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
                        Json.Decode.fail "Invalid DirectiveLocation"
            )
