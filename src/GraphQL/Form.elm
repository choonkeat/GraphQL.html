module GraphQL.Form exposing
    ( Value
    , valueFromGraphqlType
    , valueFromString
    , valueToHTML
    , valueToNamedArgs
    )

import Dict exposing (Dict)
import GraphQL
import Html exposing (Html, div, h5, hr, text)
import Html.Attributes exposing (checked, class, style, title, value)
import Html.Events exposing (onInput)
import Human
import MaybeExtra exposing (boolMap, maybeBool, maybeJoinWrap, maybeSomething, nothingIfBlank)
import UI


type Value
    = Leaf { type_ : GraphQL.Type, description : Maybe String, required : Bool, selected : Bool, value : Maybe String }
    | Nest { type_ : GraphQL.Type, description : Maybe String, required : Bool, selected : Bool } (Dict String Value)
    | Union { required : Bool, selected : Bool } (List Value)
    | Enum { type_ : GraphQL.Type, description : Maybe String, required : Bool, selected : Bool, value : Maybe String, options : List String }
    | List { required : Bool, selected : Bool } (List Value)


valueFromGraphqlType : (String -> Maybe GraphQL.Type) -> Maybe String -> Maybe String -> GraphQL.Type -> Value
valueFromGraphqlType typeLookup value description graphqlType =
    let
        fromGraphqlField graphqlField =
            Leaf
                { description = Nothing
                , type_ = GraphQL.expandedType typeLookup graphqlField.type_
                , required = maybeSomething value
                , selected = False
                , value = Nothing
                }
    in
    case graphqlType of
        GraphQL.TypeScalar attrs ->
            Leaf
                { description = description
                , type_ = graphqlType
                , required = maybeSomething value
                , selected = maybeSomething value
                , value = value
                }

        GraphQL.TypeObject attrs ->
            -- TODO: graphqlType.interfaces : Maybe (List Type)
            Nest
                { description = description
                , type_ = graphqlType
                , required = maybeSomething value
                , selected = maybeSomething value
                }
                (attrs.fields
                    |> Maybe.map
                        (List.foldl
                            (\field dict ->
                                Dict.insert field.name (fromGraphqlField field) dict
                            )
                            Dict.empty
                        )
                    |> Maybe.withDefault Dict.empty
                )

        GraphQL.TypeInterface attrs ->
            -- TODO: graphqlType.possibleTypes : Maybe (List Type)
            Nest
                { description = description
                , type_ = graphqlType
                , required = maybeSomething value
                , selected = maybeSomething value
                }
                (attrs.fields
                    |> Maybe.map
                        (List.foldl
                            (\field dict ->
                                Dict.insert field.name (fromGraphqlField field) dict
                            )
                            Dict.empty
                        )
                    |> Maybe.withDefault Dict.empty
                )

        GraphQL.TypeUnion attrs ->
            -- TODO: unsupported
            attrs.possibleTypes
                |> Maybe.map (List.map (valueFromGraphqlType typeLookup value description))
                |> Maybe.map (Union { required = maybeSomething value, selected = maybeSomething value })
                |> Maybe.withDefault (Union { required = maybeSomething value, selected = maybeSomething value } [])

        GraphQL.TypeEnum attrs ->
            Enum
                { description = description
                , type_ = graphqlType
                , required = maybeSomething value
                , selected = maybeSomething value
                , value = value
                , options =
                    attrs.enumValues
                        |> Maybe.map (List.map .name)
                        |> Maybe.withDefault []
                }

        GraphQL.TypeInput attrs ->
            Nest
                { description = description
                , type_ = graphqlType
                , required = maybeSomething value
                , selected = maybeSomething value
                }
                (attrs.inputFields
                    |> Maybe.map
                        (List.foldl
                            (\graphqlInputValue dict ->
                                GraphQL.expandedType typeLookup graphqlInputValue.type_
                                    |> valueFromGraphqlType typeLookup value graphqlInputValue.description
                                    |> (\inputValue -> Dict.insert graphqlInputValue.name inputValue dict)
                            )
                            Dict.empty
                        )
                    |> Maybe.withDefault Dict.empty
                )

        GraphQL.TypeNotNull attrs ->
            GraphQL.expandedType typeLookup attrs.ofType
                |> valueFromGraphqlType typeLookup (Just "") description

        GraphQL.TypeList attrs ->
            -- TODO:
            -- { name : Maybe String
            -- , description : Maybe String
            -- , ofType : Type
            -- }
            List { required = maybeSomething value, selected = maybeSomething value } []


valueToNamedArgs : String -> Value -> Maybe String
valueToNamedArgs key inputValue =
    valueToArg inputValue
        |> Maybe.map (\s -> key ++ ":" ++ s)


valueToArg : Value -> Maybe String
valueToArg inputValue =
    case inputValue of
        Leaf record ->
            case GraphQL.typeName record.type_ of
                Just "Boolean" ->
                    nothingIfBlank record.value

                Just "Int" ->
                    nothingIfBlank record.value

                Just "Float" ->
                    nothingIfBlank record.value

                _ ->
                    Maybe.map Debug.toString (nothingIfBlank record.value)

        Nest record inputValueStringDict ->
            Dict.map valueToNamedArgs inputValueStringDict
                |> Dict.values
                |> maybeJoinWrap " " "{" "}"

        Union record inputValueList ->
            List.map valueToArg inputValueList
                |> maybeJoinWrap " " "{" "}"

        Enum record ->
            nothingIfBlank record.value

        List record inputValueList ->
            List.map valueToArg inputValueList
                |> maybeJoinWrap ", " "[" "]"


valueFromString : List String -> String -> Value -> Value
valueFromString keys value inputValue =
    case inputValue of
        Leaf record ->
            Leaf { record | value = Just value }

        Nest record dict ->
            case keys of
                [] ->
                    inputValue

                x :: xs ->
                    Nest { record | selected = True } (Dict.update x (Maybe.map (valueFromString xs value)) dict)

        Union record inputValueList ->
            Union { record | selected = maybeBool (Just value) } (List.map (valueFromString keys value) inputValueList)

        Enum record ->
            Enum { record | value = Just value }

        List record inputValueList ->
            List { record | selected = maybeBool (Just value) } (List.map (valueFromString keys value) inputValueList)


valueToHTML : (String -> msg) -> List String -> String -> Value -> Html msg
valueToHTML setMsg keys key inputValue =
    let
        maybeInvalidClass required value =
            case ( required, maybeSomething (nothingIfBlank value) ) of
                ( True, False ) ->
                    "is-invalid"

                a ->
                    ""
    in
    case inputValue of
        Leaf record ->
            case ( record.selected, GraphQL.typeName record.type_ ) of
                -- ( False, _ ) ->
                --     notChosen key
                ( _, Just "Boolean" ) ->
                    if record.required then
                        UI.inputCheckbox
                            { label = [ text (Human.string key) ]
                            , description = UI.description record.description
                            , attrs =
                                [ checked (maybeBool record.value)
                                , value (boolMap (maybeBool record.value) "false" "true") -- opposite
                                , onInput setMsg
                                ]
                            }

                    else
                        UI.inputChoices setMsg
                            { required = record.required
                            , label = [ text (Human.string key) ]
                            , description = UI.description record.description
                            , attrs = [ class ("custom-select " ++ maybeInvalidClass record.required record.value) ]
                            , options = [ "true", "false" ]
                            }

                ( _, Just "Int" ) ->
                    UI.inputString
                        { label = [ text (Human.string key) ]
                        , htmlType = "number"
                        , value = Maybe.withDefault "" record.value
                        , description = UI.description record.description
                        , attrs =
                            [ class ("form-control " ++ maybeInvalidClass record.required record.value)
                            , onInput setMsg
                            ]
                        }

                ( _, Just "Float" ) ->
                    UI.inputString
                        { label = [ text (Human.string key) ]
                        , htmlType = "number"
                        , value = Maybe.withDefault "" record.value
                        , description = UI.description record.description
                        , attrs =
                            [ class ("form-control " ++ maybeInvalidClass record.required record.value)
                            , onInput setMsg
                            ]
                        }

                _ ->
                    UI.inputString
                        { label = [ text (Human.string key) ]
                        , htmlType = "text"
                        , value = Maybe.withDefault "" record.value
                        , description = UI.description record.description
                        , attrs =
                            [ class ("form-control " ++ maybeInvalidClass record.required record.value)
                            , onInput setMsg
                            ]
                        }

        Nest record inputValueStringDict ->
            div
                [ class "card mb-3" ]
                [ h5 [ class "card-header", style "text-transform" "capitalize", title (Debug.toString record) ] [ text (Human.string key) ]
                , div [ class "card-body " ]
                    [ text (Maybe.withDefault "" record.description)
                    , div [] (Dict.values (Dict.map (valueToHTML setMsg (List.append keys [ key ])) inputValueStringDict))
                    ]
                , div [ class "col" ] [ UI.description record.description ]
                ]

        Union record inputValueList ->
            div []
                (List.intersperse (hr [] []) (List.map (valueToHTML setMsg keys key) inputValueList))

        Enum record ->
            UI.inputChoices setMsg
                { required = record.required
                , label = [ text (Human.string key) ]
                , description = UI.description record.description
                , attrs = [ class ("custom-select " ++ maybeInvalidClass record.required record.value) ]
                , options = record.options
                }

        List record inputValueList ->
            div []
                (List.intersperse (hr [] []) (List.map (valueToHTML setMsg keys key) inputValueList))
