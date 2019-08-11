module Route exposing (Route(..), fromUrl, route)

import Base64
import Url
import Url.Parser exposing ((</>), (<?>), Parser, fragment, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query


type alias Query =
    { p : Maybe String
    , q : Maybe String
    }


decodeQuery : Url.Parser.Query.Parser Query
decodeQuery =
    Url.Parser.Query.map2 Query
        (Url.Parser.Query.string "p")
        (Url.Parser.Query.string "q")


type Route
    = NotFound
    | Homepage Query
    | OperationTypes String
    | SelectionSets String String
    | Request String String String


base64string : Url.Parser.Parser (String -> a) a
base64string =
    Url.Parser.custom "BASE64STRING" (Base64.decode >> Result.toMaybe)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Homepage (top <?> decodeQuery)
        , map OperationTypes base64string
        , map SelectionSets (base64string </> string)
        , map Request (base64string </> string </> string)
        ]


fromUrl : Url.Url -> Route
fromUrl urlUrl =
    Url.Parser.parse route urlUrl
        |> Maybe.withDefault NotFound
