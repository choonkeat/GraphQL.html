module API exposing
    ( httpJsonBodyResolver
    , httpStringBodyResolver
    , introspect
    )

import GraphQL
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)


introspect : String -> Task Http.Error String
introspect targetURL =
    Http.task
        { method = "POST"
        , headers = []
        , url = targetURL
        , body = Http.jsonBody (Json.Encode.object [ ( "query", Json.Encode.string GraphQL.introspectionQuery ) ])
        , resolver = Http.stringResolver httpStringBodyResolver
        , timeout = Just 15000
        }



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
            Json.Decode.decodeString decoder s
                -- just trying; if our decoder understands the response body, great
                |> Result.mapError (\_ -> Http.BadStatus m.statusCode)
