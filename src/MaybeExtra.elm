module MaybeExtra exposing (..)


boolMap : Bool -> a -> a -> a
boolMap bool true false =
    if bool then
        true

    else
        false


maybeBool : Maybe String -> Bool
maybeBool boolMaybe =
    case boolMaybe of
        Just "true" ->
            True

        _ ->
            False


maybeSomething : Maybe a -> Bool
maybeSomething =
    Maybe.map (\_ -> True) >> Maybe.withDefault False


listWithoutNothing : List (Maybe a) -> List a
listWithoutNothing =
    List.foldl (\m sum -> List.append sum (Maybe.withDefault [] (Maybe.map (\a -> [ a ]) m))) []


nothingIfBlank : Maybe String -> Maybe String
nothingIfBlank stringMaybe =
    case stringMaybe of
        Just "" ->
            Nothing

        _ ->
            stringMaybe


maybeJoinWrap : String -> String -> String -> List (Maybe String) -> Maybe String
maybeJoinWrap connect start end listMaybes =
    case listWithoutNothing listMaybes of
        [] ->
            Nothing

        list ->
            Just (start ++ String.join connect list ++ end)
