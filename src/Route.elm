module Route exposing (Route(..), fromUrl, route)

import Url
import Url.Parser exposing ((</>), Parser, fragment, int, map, oneOf, parse, s, string, top)


type Route
    = NotFound
    | APIs
    | OperationTypes String
    | SelectionSets String String
    | Request String String String


route : Parser (Route -> a) a
route =
    oneOf
        [ map APIs top
        , map OperationTypes string
        , map SelectionSets (string </> string)
        , map Request (string </> string </> string)
        ]


fromUrl : Url.Url -> Route
fromUrl urlUrl =
    Url.Parser.parse route urlUrl
        |> Maybe.withDefault NotFound
