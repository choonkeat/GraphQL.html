module Route exposing (Route(..), fromUrl, route)

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


route : Parser (Route -> a) a
route =
    oneOf
        [ map Homepage (top <?> decodeQuery)
        , map OperationTypes string
        , map SelectionSets (string </> string)
        , map Request (string </> string </> string)
        ]


fromUrl : Url.Url -> Route
fromUrl urlUrl =
    Url.Parser.parse route urlUrl
        |> Maybe.withDefault NotFound
