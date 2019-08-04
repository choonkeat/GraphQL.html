module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import API
import Array exposing (Array)
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import GraphQL exposing (typeName)
import Html exposing (Html, a, button, div, form, h1, h3, h5, hr, input, label, main_, nav, node, option, p, pre, select, small, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, for, href, id, name, placeholder, rel, required, style, title, type_, value)
import Html.Events exposing (on, onBlur, onClick, onInput)
import Http
import Json.Decode
import List exposing (sum)
import Task exposing (Task)
import Templates
import Time
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Alert =
    { category : String
    , message : String
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , alert : Maybe Alert
    , schema : Maybe GraphQL.Schema
    , types : Dict String GraphQL.Type
    , type_ : Maybe GraphQL.Type
    , field : Maybe GraphQL.Field
    , selectionSet : Maybe SelectionSet
    , apiURL : String
    }


type alias Flags =
    { offsetInMinutes : Int
    , nowMilliseconds : Int
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | ModelChanged (Model -> String -> Model) String
    | OnHttpResponse (Result Http.Error String)
    | ChosenSchema String GraphQL.Field
    | ApiUrlUpdated


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { navKey = navKey
            , alert = Nothing
            , schema = Nothing
            , types = Dict.empty
            , type_ = Nothing
            , field = Nothing
            , apiURL = "https://metaphysics-production.artsy.net/"
            , selectionSet = Nothing
            }
    in
    ( model
    , Task.succeed Templates.artsyJson
        -- |> Task.andThen (\_ -> API.introspect model.apiURL)
        |> Task.attempt OnHttpResponse
    )


view : Model -> Browser.Document Msg
view model =
    let
        typeLookup s =
            Dict.get s model.types
    in
    Browser.Document "App"
        [ main_ [ class "container" ]
            [ div [ class "mt-3" ]
                []
            , viewAlert model.alert
            , form []
                [ div [ class "form-group" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , value model.apiURL
                        , onBlur ApiUrlUpdated
                        , onInput (ModelChanged (\m s -> { m | apiURL = s }))
                        ]
                        []
                    ]
                ]
            , div [ class "row mt-5" ]
                [ div [ class "col-3", style "word-break" "break-all" ]
                    (List.map
                        (\( heading, maybeHeadingType ) ->
                            maybeHeadingType
                                |> Maybe.map
                                    (\headingType ->
                                        div [ class "card mb-3" ]
                                            [ h5 [ class "card-header", style "text-transform" "capitalize" ] [ text heading ]
                                            , viewQueriesNav heading (GraphQL.fields model.types headingType)
                                            ]
                                    )
                                |> Maybe.withDefault (text "")
                        )
                        [ ( "query", Maybe.map .queryType model.schema )
                        , ( "mutation", Maybe.andThen .mutationType model.schema )
                        , ( "subscription", Maybe.andThen .subscriptionType model.schema )
                        ]
                    )
                , div [ class "col-9" ]
                    [ case model.selectionSet of
                        Just (SelectionNest record form) ->
                            div []
                                [ h3 [ style "text-transform" "capitalize" ] [ text (humanize record.field.name) ]
                                , renderForm typeLookup [] record.field.name form
                                , pre [ class "p-4", style "white-space" "pre-wrap", style "background-color" "lightgray" ]
                                    [ text (formQuery record.field.name form) ]
                                ]

                        _ ->
                            text ""
                    ]
                ]
            ]
        ]


selectionSetName : SelectionSet -> Maybe String
selectionSetName selectionSet =
    case selectionSet of
        SelectionLeaf record ->
            Nothing

        SelectionNest record form ->
            Just record.field.name

        SelectionPending record ->
            Just record.field.name


selectionSetForm : SelectionSet -> Maybe Form
selectionSetForm selectionSet =
    case selectionSet of
        SelectionLeaf record ->
            Nothing

        SelectionNest record form ->
            Just form

        SelectionPending record ->
            Nothing


viewAlert : Maybe Alert -> Html Msg
viewAlert alertMaybe =
    case alertMaybe of
        Just alert ->
            div [ class "row-fluid mt-5" ]
                [ div [ class ("alert alert-" ++ alert.category) ] [ text alert.message ] ]

        Nothing ->
            text ""


viewQueriesNav : String -> List GraphQL.Field -> Html Msg
viewQueriesNav heading list =
    div [ class "list-group-flush", style "overflow-y" "scroll", style "height" "20em", style "margin-top" "-1px" ]
        (List.map (viewSchemaType heading) list)


viewSchemaType : String -> GraphQL.Field -> Html Msg
viewSchemaType heading field =
    a
        [ class "list-group-item list-group-item-action"
        , href "#"
        , onClick (ChosenSchema heading field)
        ]
        [ h5 [ class "mb-1" ] [ text field.name ]
        , p [ class "mb-1" ] [ text (Maybe.withDefault "" field.description) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- [url] decide what to do
        OnUrlRequest (Browser.Internal urlUrl) ->
            ( model, Browser.Navigation.pushUrl model.navKey (Url.toString urlUrl) )

        OnUrlRequest (Browser.External urlString) ->
            ( model, Browser.Navigation.load urlString )

        -- [url] given that we _are at this url_ how should our model change?
        OnUrlChange urlUrl ->
            ( model, Cmd.none )

        ModelChanged function string ->
            ( function model string, Cmd.none )

        OnHttpResponse (Ok string) ->
            case Json.Decode.decodeString GraphQL.decodeResponse string of
                Err oops ->
                    ( { model | alert = Just { category = "danger", message = Json.Decode.errorToString oops } }, Cmd.none )

                Ok resp ->
                    let
                        newTypes =
                            GraphQL.typesDict resp.data.schema.types Dict.empty
                    in
                    ( { model | alert = Nothing, schema = Just resp.data.schema, types = newTypes }, Cmd.none )

        OnHttpResponse (Err oops) ->
            ( { model | alert = Just { category = "danger", message = Debug.toString oops } }, Cmd.none )

        ChosenSchema heading field ->
            let
                typeLookup s =
                    Dict.get s model.types

                newType =
                    GraphQL.fieldType model.types field

                newField =
                    Just field

                newSelectionSet =
                    case newType of
                        Nothing ->
                            model.selectionSet

                        Just t ->
                            Just (SelectionNest { type_ = t, field = field, selected = True } (graphqlFieldToForm typeLookup field field.description))
            in
            ( { model
                | type_ = newType
                , field = newField
                , selectionSet = newSelectionSet
              }
            , Cmd.none
            )

        ApiUrlUpdated ->
            ( model
            , API.introspect model.apiURL
                |> Task.attempt OnHttpResponse
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--
--
--
--


type SelectionSet
    = SelectionLeaf { type_ : GraphQL.Type, description : Maybe String, selected : Bool }
    | SelectionNest { type_ : GraphQL.Type, field : GraphQL.Field, selected : Bool } Form
    | SelectionPending { type_ : GraphQL.Type, field : GraphQL.Field, selected : Bool }


type InputValue
    = InputLeaf { type_ : GraphQL.Type, description : Maybe String, value : Maybe String }
    | InputNest { type_ : GraphQL.Type, description : Maybe String, selected : Bool } (Dict String InputValue)
    | InputUnion (List InputValue)
    | InputEnum { type_ : GraphQL.Type, description : Maybe String, value : Maybe String, options : List String }
    | InputList (List InputValue)


type alias Form =
    { description : Maybe String
    , inputValues : Dict String InputValue
    , selectionSet : Dict String SelectionSet
    }


formDictQuery : String -> Dict String Form -> String
formDictQuery key dict =
    String.join "\n"
        ((key ++ " {") :: List.append (Dict.values (Dict.map formQuery dict)) [ "}" ])


formQuery : String -> Form -> String
formQuery key form =
    let
        queries =
            Dict.map inputValueQuery form.inputValues
                |> Dict.values
                |> listWithoutNothing

        selectionSetOutput =
            "{\n" ++ selectionSetDictQuery form.selectionSet ++ "\n}"
    in
    case queries of
        [] ->
            key ++ " " ++ selectionSetOutput

        _ ->
            String.join "\n" ((key ++ "(") :: List.append queries [ ") " ++ selectionSetOutput ])


selectionSetDictQuery : Dict String SelectionSet -> String
selectionSetDictQuery dict =
    Dict.map selectionSetQuery dict
        |> Dict.values
        |> listWithoutNothing
        |> String.join "\n"


selectionSetQuery : String -> SelectionSet -> Maybe String
selectionSetQuery key selectionSet =
    case selectionSet of
        SelectionLeaf record ->
            if record.selected then
                Just key

            else
                Nothing

        SelectionNest record form ->
            if record.selected then
                Just (formQuery key form)

            else
                Nothing

        SelectionPending record ->
            Nothing


inputValueQuery : String -> InputValue -> Maybe String
inputValueQuery key inputValue =
    inputValueQueryValue inputValue
        |> Maybe.map (\s -> key ++ ":" ++ s)


inputValueQueryValue : InputValue -> Maybe String
inputValueQueryValue inputValue =
    case inputValue of
        InputLeaf record ->
            case typeName record.type_ of
                Just "Boolean" ->
                    record.value

                _ ->
                    Maybe.map Debug.toString record.value

        InputNest record inputValueStringDict ->
            Dict.map inputValueQuery inputValueStringDict
                |> Dict.values
                |> maybeJoinWrap " " "{" "}"

        InputUnion inputValueList ->
            List.map inputValueQueryValue inputValueList
                |> maybeJoinWrap " " "{" "}"

        InputEnum record ->
            record.value

        InputList inputValueList ->
            List.map inputValueQueryValue inputValueList
                |> maybeJoinWrap ", " "[" "]"


maybeJoinWrap : String -> String -> String -> List (Maybe String) -> Maybe String
maybeJoinWrap connect start end listMaybes =
    case listWithoutNothing listMaybes of
        [] ->
            Nothing

        list ->
            Just (start ++ String.join connect list ++ end)



-- Just (String.join ", " (List.map (inputValueQuery key) inputValueList))


renderForm : (String -> Maybe GraphQL.Type) -> List String -> String -> Form -> Html Msg
renderForm typeLookup keys key form =
    let
        bodyFooter =
            [ if form.inputValues == Dict.empty then
                text ""

              else
                div [ class "card-body" ] (Dict.values (Dict.map (renderInputValue (List.append keys [ key ])) form.inputValues))
            , div [ class "card-footer" ]
                [ div [ class "form-group form-check" ] (Dict.values (Dict.map (chooseSelectionSet typeLookup (List.append keys [ key ])) form.selectionSet))
                ]
            ]
    in
    if keys == [] then
        div [ class "card mb-3" ]
            (h5 [ class "card-header", style "text-transform" "capitalize" ] [ text key ]
                :: bodyFooter
            )

    else
        div [] bodyFooter


chooseSelectionSet : (String -> Maybe GraphQL.Type) -> List String -> String -> SelectionSet -> Html Msg
chooseSelectionSet typeLookup keys key selectionSet =
    let
        fieldName =
            String.join "-" keys ++ "-" ++ key
    in
    case selectionSet of
        SelectionLeaf record ->
            div []
                [ input
                    [ type_ "checkbox"
                    , id fieldName
                    , class "form-check-input"
                    , checked record.selected
                    , value (boolMap record.selected "false" "true") -- opposite
                    , onInput (ModelChanged (toggleModelSelectionSet typeLookup (List.append keys [ key ])))
                    ]
                    []
                , label [ for fieldName ]
                    [ text key
                    , div [] [ htmlDescription record.description ]
                    ]
                ]

        SelectionNest record form ->
            div []
                [ input
                    [ type_ "checkbox"
                    , id fieldName
                    , class "form-check-input"
                    , checked record.selected
                    , value (boolMap record.selected "false" "true") -- opposite
                    , onInput (ModelChanged (toggleModelSelectionSet typeLookup (List.append keys [ key ])))
                    ]
                    []
                , label [ for fieldName ] [ text key ]
                , renderForm typeLookup keys key form
                ]

        SelectionPending record ->
            div [ class "text-muted d-block" ]
                [ input
                    [ type_ "checkbox"
                    , id fieldName
                    , class "form-check-input"
                    , checked False
                    , value "true" -- opposite
                    , onInput (ModelChanged (toggleModelSelectionSet typeLookup (List.append keys [ key ])))
                    ]
                    []
                , label [ for fieldName ]
                    [ text key
                    , div [] [ htmlDescription record.field.description ]
                    ]
                ]


toggleModelSelectionSet : (String -> Maybe GraphQL.Type) -> List String -> Model -> String -> Model
toggleModelSelectionSet typeLookup keys model value =
    case model.selectionSet of
        Just (SelectionNest record form) ->
            { model | selectionSet = Just (toggleSelectionSet typeLookup (List.drop 1 keys) (SelectionNest record form)) }

        _ ->
            model


toggleSelectionSet : (String -> Maybe GraphQL.Type) -> List String -> SelectionSet -> SelectionSet
toggleSelectionSet typeLookup keys selectionSet =
    case selectionSet of
        SelectionLeaf record ->
            SelectionLeaf { record | selected = not record.selected }

        SelectionNest record form ->
            case keys of
                [] ->
                    if record.selected then
                        SelectionPending { type_ = record.type_, field = record.field, selected = False }

                    else
                        SelectionNest { record | selected = not record.selected } form

                x :: xs ->
                    let
                        newForm =
                            { form | selectionSet = Dict.update x (Maybe.map (toggleSelectionSet typeLookup xs)) form.selectionSet }
                    in
                    SelectionNest record newForm

        SelectionPending record ->
            graphqlFieldToSelectionSet
                typeLookup
                record.field.description
                record.field.name
                record.type_
                record.field
                True


setModelFormValue : List String -> Model -> String -> Model
setModelFormValue keys model value =
    case model.selectionSet of
        Just (SelectionNest record form) ->
            let
                newForm =
                    setFormValue (List.drop 1 keys) form value
            in
            { model | selectionSet = Just (SelectionNest record newForm) }

        _ ->
            model


setFormValue : List String -> Form -> String -> Form
setFormValue keys form value =
    case keys of
        x :: xs ->
            case Dict.get x form.inputValues of
                Just _ ->
                    { form | inputValues = Dict.update x (Maybe.map (setInputValue xs value)) form.inputValues }

                Nothing ->
                    { form | selectionSet = Dict.update x (Maybe.map (setSelectionSetValue xs value)) form.selectionSet }

        _ ->
            form


setSelectionSetValue : List String -> String -> SelectionSet -> SelectionSet
setSelectionSetValue keys value selectionSet =
    let
        unexpectedSelectionSet =
            let
                _ =
                    Debug.log "UNEXPECTED setSelectionSetValue" ( keys, value, selectionSet )
            in
            selectionSet
    in
    case selectionSet of
        SelectionLeaf record ->
            unexpectedSelectionSet

        SelectionNest record form ->
            SelectionNest record (setFormValue keys form value)

        SelectionPending record ->
            unexpectedSelectionSet


setInputValue : List String -> String -> InputValue -> InputValue
setInputValue keys value inputValue =
    case inputValue of
        InputLeaf record ->
            InputLeaf { record | value = Just value }

        InputNest record dict ->
            case keys of
                [] ->
                    inputValue

                x :: xs ->
                    InputNest { record | selected = True } (Dict.update x (Maybe.map (setInputValue xs value)) dict)

        InputUnion inputValueList ->
            InputUnion (List.map (setInputValue keys value) inputValueList)

        InputEnum record ->
            InputEnum { record | value = Just value }

        InputList inputValueList ->
            InputList (List.map (setInputValue keys value) inputValueList)


renderInputValue : List String -> String -> InputValue -> Html Msg
renderInputValue keys key inputValue =
    case inputValue of
        InputLeaf record ->
            case typeName record.type_ of
                Just "Boolean" ->
                    let
                        fieldName =
                            String.join "-" keys ++ "-" ++ key
                    in
                    div [ class "form-group form-check" ]
                        [ input
                            [ type_ "checkbox"
                            , id fieldName
                            , class "form-check-input"
                            , checked (maybeBool record.value)
                            , value (boolMap (maybeBool record.value) "false" "true") -- opposite
                            , onInput (ModelChanged (setModelFormValue (List.append keys [ key ])))
                            ]
                            []
                        , label [ for fieldName, style "text-transform" "capitalize", title (Debug.toString record) ] [ text (humanize key) ]
                        , div [] [ htmlDescription record.description ]
                        ]

                _ ->
                    div [ class "form-group" ]
                        [ label [ style "text-transform" "capitalize", title (Debug.toString record) ] [ text (humanize key) ]
                        , input
                            [ type_ "text"
                            , class "form-control"
                            , value (Maybe.withDefault "" record.value)
                            , onInput (ModelChanged (setModelFormValue (List.append keys [ key ])))
                            ]
                            []
                        , div [] [ htmlDescription record.description ]
                        ]

        InputNest record inputValueStringDict ->
            div [ class "card mb-3" ]
                [ h5 [ class "card-header", style "text-transform" "capitalize", title (Debug.toString record) ] [ text (humanize key) ]
                , div [ class "card-body " ]
                    [ text (Maybe.withDefault "" record.description)
                    , div [] (Dict.values (Dict.map (renderInputValue (List.append keys [ key ])) inputValueStringDict))
                    ]
                , div [] [ htmlDescription record.description ]
                ]

        InputUnion inputValueList ->
            div []
                (List.intersperse (hr [] []) (List.map (renderInputValue keys key) inputValueList))

        InputEnum record ->
            div [ class "form-group" ]
                [ label [ style "text-transform" "capitalize", title (Debug.toString record) ] [ text (humanize key) ]
                , select [ class "custom-select", on "change" (decodeSelectChanged (List.append keys [ key ])) ]
                    (List.append
                        (case record.value of
                            Nothing ->
                                [ option [] [ text "Choose..." ] ]

                            Just s ->
                                []
                        )
                        (List.map (\s -> option [ value s ] [ text (humanize s) ]) record.options)
                    )
                , div [] [ htmlDescription record.description ]
                ]

        InputList inputValueList ->
            div []
                (List.intersperse (hr [] []) (List.map (renderInputValue keys key) inputValueList))


decodeSelectChanged : List String -> Json.Decode.Decoder Msg
decodeSelectChanged keys =
    Json.Decode.map (ModelChanged (setModelFormValue keys)) Html.Events.targetValue


graphqlFieldToForm : (String -> Maybe GraphQL.Type) -> GraphQL.Field -> Maybe String -> Form
graphqlFieldToForm typeLookup graphqlField description =
    -- type alias Field =
    --     { name : String
    --     , description : Maybe String
    --     , args : List InputValue
    --     , type_ : Type
    --     , isDeprecated : Bool
    --     , deprecationReason : Maybe String
    --     }
    { description = description
    , inputValues =
        List.foldl
            (\arg dict ->
                Dict.insert arg.name
                    (graphqlTypeToInputValue typeLookup Nothing arg.description (absoluteType typeLookup arg.type_))
                    dict
            )
            Dict.empty
            graphqlField.args
    , selectionSet =
        absoluteType typeLookup graphqlField.type_
            |> graphqlTypeToSelectionSet typeLookup graphqlField.description graphqlField.name
    }


graphqlFieldToSelectionSet : (String -> Maybe GraphQL.Type) -> Maybe String -> String -> GraphQL.Type -> GraphQL.Field -> Bool -> SelectionSet
graphqlFieldToSelectionSet typeLookup description name graphqlType graphqlField selected =
    if selected then
        -- SelectionNest
        --     { type_ = graphqlType, field = graphqlField, selected = selected }
        --     (graphqlFieldToForm typeLookup graphqlField graphqlField.description)
        case graphqlType of
            GraphQL.TypeScalar attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeObject attrs ->
                SelectionNest
                    { type_ = graphqlType, field = graphqlField, selected = selected }
                    (graphqlFieldToForm typeLookup graphqlField graphqlField.description)

            GraphQL.TypeInterface attrs ->
                SelectionNest
                    { type_ = graphqlType, field = graphqlField, selected = selected }
                    (graphqlFieldToForm typeLookup graphqlField graphqlField.description)

            GraphQL.TypeUnion attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeEnum attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeInput attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeNotNull attrs ->
                graphqlFieldToSelectionSet typeLookup description name attrs.ofType graphqlField selected

            GraphQL.TypeList attrs ->
                graphqlFieldToSelectionSet typeLookup description name attrs.ofType graphqlField selected

    else
        SelectionPending
            { type_ = graphqlType, field = graphqlField, selected = selected }


graphqlTypeToSelectionSet : (String -> Maybe GraphQL.Type) -> Maybe String -> String -> GraphQL.Type -> Dict String SelectionSet
graphqlTypeToSelectionSet typeLookup description name graphqlType =
    let
        -- given `attrs`, return a Dict of single key-value
        singleNameSelection attrs =
            Dict.fromList [ ( name, SelectionLeaf { type_ = graphqlType, selected = False, description = attrs.description } ) ]

        -- insert field+type into a dict
        insertNameSelection dict field type_ =
            Dict.insert
                field.name
                (graphqlFieldToSelectionSet typeLookup description name type_ field False)
                dict

        -- fold a `List Field` into `Dict String SelectionSet`
        foldlNameSelection fields =
            List.foldl
                (\field dict ->
                    absoluteType typeLookup field.type_
                        |> insertNameSelection dict field
                )
                Dict.empty
                fields
    in
    case graphqlType of
        GraphQL.TypeScalar attrs ->
            singleNameSelection attrs

        GraphQL.TypeObject attrs ->
            attrs.fields
                |> Maybe.map foldlNameSelection
                |> Maybe.withDefault Dict.empty

        GraphQL.TypeInterface attrs ->
            attrs.fields
                |> Maybe.map foldlNameSelection
                |> Maybe.withDefault Dict.empty

        GraphQL.TypeUnion attrs ->
            singleNameSelection attrs

        GraphQL.TypeEnum attrs ->
            singleNameSelection attrs

        GraphQL.TypeInput attrs ->
            singleNameSelection attrs

        GraphQL.TypeNotNull attrs ->
            absoluteType typeLookup attrs.ofType
                |> graphqlTypeToSelectionSet typeLookup description name

        GraphQL.TypeList attrs ->
            absoluteType typeLookup attrs.ofType
                |> graphqlTypeToSelectionSet typeLookup description name


graphqlTypeToInputValue : (String -> Maybe GraphQL.Type) -> Maybe String -> Maybe String -> GraphQL.Type -> InputValue
graphqlTypeToInputValue typeLookup value description graphqlType =
    case graphqlType of
        GraphQL.TypeScalar attrs ->
            InputLeaf { description = description, type_ = graphqlType, value = value }

        GraphQL.TypeObject attrs ->
            -- TODO: graphqlType.interfaces : Maybe (List Type)
            InputNest
                { description = description, type_ = graphqlType, selected = maybeSomething value }
                -- (Dict String InputValue)
                (attrs.fields
                    |> Maybe.map
                        (List.foldl
                            (\field dict ->
                                Dict.insert field.name (graphqlFieldToInputType typeLookup value field) dict
                            )
                            Dict.empty
                        )
                    |> Maybe.withDefault Dict.empty
                )

        GraphQL.TypeInterface attrs ->
            -- TODO: graphqlType.possibleTypes : Maybe (List Type)
            InputNest
                { description = description, type_ = graphqlType, selected = maybeSomething value }
                -- (Dict String InputValue)
                (attrs.fields
                    |> Maybe.map
                        (List.foldl
                            (\field dict ->
                                Dict.insert field.name (graphqlFieldToInputType typeLookup value field) dict
                            )
                            Dict.empty
                        )
                    |> Maybe.withDefault Dict.empty
                )

        GraphQL.TypeUnion attrs ->
            attrs.possibleTypes
                |> Maybe.map (List.map (graphqlTypeToInputValue typeLookup value description))
                |> Maybe.map InputUnion
                |> Maybe.withDefault (InputUnion [])

        GraphQL.TypeEnum attrs ->
            InputEnum
                { description = description
                , type_ = graphqlType
                , value = value
                , options =
                    attrs.enumValues
                        |> Maybe.map (List.map .name)
                        |> Maybe.withDefault []
                }

        GraphQL.TypeInput attrs ->
            InputNest
                { description = description
                , type_ = graphqlType
                , selected = maybeSomething value
                }
                -- (Dict String InputValue)
                (attrs.inputFields
                    |> Maybe.map
                        (List.foldl
                            (\graphqlInputValue dict ->
                                absoluteType typeLookup graphqlInputValue.type_
                                    |> graphqlTypeToInputValue typeLookup value graphqlInputValue.description
                                    |> (\inputValue -> Dict.insert graphqlInputValue.name inputValue dict)
                            )
                            Dict.empty
                        )
                    |> Maybe.withDefault Dict.empty
                )

        GraphQL.TypeNotNull attrs ->
            absoluteType typeLookup attrs.ofType
                |> graphqlTypeToInputValue typeLookup (Just "") description

        GraphQL.TypeList attrs ->
            -- TODO:
            -- { name : Maybe String
            -- , description : Maybe String
            -- , ofType : Type
            -- }
            InputList []


graphqlFieldToInputType : (String -> Maybe GraphQL.Type) -> Maybe String -> GraphQL.Field -> InputValue
graphqlFieldToInputType typeLookup value graphqlField =
    -- GraphQL.Field = { name : String
    -- , description : Maybe String
    -- , args : List InputValue
    -- , type_ : Type
    -- , isDeprecated : Bool
    -- , deprecationReason : Maybe String
    -- }
    InputLeaf
        { description = Just "graphqlFieldToInputType"
        , type_ = absoluteType typeLookup graphqlField.type_
        , value = Nothing
        }


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
maybeSomething aMaybe =
    Maybe.withDefault False (Maybe.map (\_ -> True) aMaybe)


listWithoutNothing : List (Maybe a) -> List a
listWithoutNothing =
    List.foldl (\m sum -> List.append sum (Maybe.withDefault [] (Maybe.map (\a -> [ a ]) m))) []


humanize : String -> String
humanize string =
    if String.endsWith "_id" string then
        String.dropRight 3 (String.replace "_" " " string) ++ " ID"

    else
        String.replace "_" " " string


htmlDescription : Maybe String -> Html a
htmlDescription maybeString =
    small [ class "text-muted" ] [ text (Maybe.withDefault "" maybeString) ]


maybeRender : (a -> Html b) -> Maybe a -> Html b
maybeRender function aMaybe =
    Maybe.map function aMaybe
        |> Maybe.withDefault (text "")


absoluteType : (String -> Maybe GraphQL.Type) -> GraphQL.Type -> GraphQL.Type
absoluteType typeLookup type_ =
    typeName type_
        |> Maybe.andThen typeLookup
        |> Maybe.withDefault type_
