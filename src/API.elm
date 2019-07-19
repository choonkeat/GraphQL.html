module API exposing
    ( httpJsonBodyResolver
    , httpStringBodyResolver
    , introspect
    , introspectionQuery
    )

import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Types


introspect : String -> Task Http.Error Types.DataSchema
introspect targetURL =
    Http.task
        { method = "POST"
        , headers = []
        , url = targetURL
        , body = Http.jsonBody (Json.Encode.object [ ( "query", Json.Encode.string introspectionQuery ) ])
        , resolver = Http.stringResolver (httpJsonBodyResolver (Types.decodeIntrospected |> Json.Decode.map (\a -> a.data.schema)))
        , timeout = Just 5000
        }


introspectionQuery : String
introspectionQuery =
    """
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
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



--


{-| to obtain a String from `Http.task`
Http.task
{ resolver = Http.stringResolver httpStringBodyResolver
, ...
}
-}
httpStringBodyResolver : Http.Response String -> Result Http.Error String
httpStringBodyResolver resp =
    case resp of
        Http.GoodStatus_ m s ->
            Ok s

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            Err (Http.BadStatus m.statusCode)


{-| applies a json decoder to response of `Http.task`
Http.task
{ resolver = Http.stringResolver (httpJsonBodyResolver thingDecoder)
, ...
}
-}
httpJsonBodyResolver : Json.Decode.Decoder a -> Http.Response String -> Result Http.Error a
httpJsonBodyResolver decoder resp =
    case resp of
        Http.GoodStatus_ m s ->
            Json.Decode.decodeString decoder s
                |> Result.mapError (Json.Decode.errorToString >> Http.BadBody)

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            Err (Http.BadStatus m.statusCode)
