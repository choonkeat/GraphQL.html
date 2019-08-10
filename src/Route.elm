module Route exposing (Route(..), fromUrl, route)

import Const
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
    let
        pathPrefix =
            if String.startsWith "/" Const.pathPrefix then
                String.dropLeft 1 Const.pathPrefix

            else
                Const.pathPrefix
    in
    oneOf
        [ map APIs top
        , map APIs (s pathPrefix)
        , map OperationTypes (s pathPrefix </> string)
        , map SelectionSets (s pathPrefix </> string </> string)
        , map Request (s pathPrefix </> string </> string </> string)
        ]


fromUrl : Url.Url -> Route
fromUrl urlUrl =
    Url.Parser.parse route urlUrl
        |> Maybe.withDefault NotFound
