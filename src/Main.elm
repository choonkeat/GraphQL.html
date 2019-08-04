module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import API
import Array exposing (Array)
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import GraphQL exposing (typeName)
import Html exposing (Html, a, button, div, form, h1, h3, h5, hr, input, label, main_, nav, node, option, p, pre, select, small, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, for, href, id, name, placeholder, rel, required, style, title, type_, value)
import Html.Events exposing (on, onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Json.Encode
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
    , selectionKey : String
    , selection : Maybe Selection
    , selectionResult : Maybe String
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
    | FormSubmitted
    | OnFormResponse (Result Http.Error String)


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
            , selectionKey = ""
            , selection = Nothing
            , selectionResult = Nothing
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
                    [ case model.selection of
                        Just (SelectionNest record) ->
                            div [ class "row" ]
                                [ form [ class "col-6", onSubmit FormSubmitted ]
                                    [ renderForm typeLookup [] record.field.name record
                                    , button [ type_ "submit", class "btn btn-primary" ] [ text "Submit" ]
                                    ]
                                , pre [ class "col-6 pt-3 pb-3", style "white-space" "pre-wrap", style "background-color" "lightgray" ]
                                    [ text (formQuery record.field.name record)
                                    , text "\n\n"
                                    , text (Maybe.withDefault "" model.selectionResult)
                                    ]
                                ]

                        _ ->
                            text ""
                    ]
                ]
            ]
        ]


selectionName : Selection -> Maybe String
selectionName selection =
    case selection of
        SelectionLeaf record ->
            Nothing

        SelectionNest record ->
            Just record.field.name

        SelectionPending record ->
            Just record.field.name


selectionForm : Selection -> Maybe SelectionNestAttrs
selectionForm selection =
    case selection of
        SelectionLeaf record ->
            Nothing

        SelectionNest record ->
            Just record

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
            case Json.Decode.decodeString GraphQL.decodeIntrospectResponse string of
                Err oops ->
                    ( { model | alert = Just { category = "danger", message = Json.Decode.errorToString oops } }, Cmd.none )

                Ok resp ->
                    let
                        newTypes =
                            GraphQL.typesDict resp.data.schema.types Dict.empty
                    in
                    ( { model | alert = Nothing, schema = Just resp.data.schema, types = newTypes }, Cmd.none )

        OnHttpResponse (Err err) ->
            ( { model | alert = Just { category = "danger", message = Debug.toString err } }, Cmd.none )

        ChosenSchema heading field ->
            let
                typeLookup s =
                    Dict.get s model.types

                newType =
                    GraphQL.fieldType model.types field

                newField =
                    Just field

                newSelection =
                    case newType of
                        Nothing ->
                            model.selection

                        Just t ->
                            Just (SelectionNest (graphqlFieldToForm typeLookup t field True))
            in
            ( { model
                | type_ = newType
                , field = newField
                , selectionKey = heading
                , selection = newSelection
              }
            , Cmd.none
            )

        ApiUrlUpdated ->
            ( model
            , API.introspect model.apiURL
                |> Task.attempt OnHttpResponse
            )

        FormSubmitted ->
            case model.selection of
                Just selection ->
                    ( model
                    , httpRequest model.apiURL selection
                        |> Task.attempt OnFormResponse
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnFormResponse (Ok data) ->
            ( { model | selectionResult = Just data }, Cmd.none )

        OnFormResponse (Err err) ->
            ( { model | alert = Just { category = "danger", message = Debug.toString err } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--
--
--
--


httpRequest : String -> Selection -> Task Http.Error String
httpRequest apiEndpoint selection =
    case selectionQuery "" selection of
        Nothing ->
            Task.fail (Http.BadBody "Incomplete request form")

        Just query ->
            let
                httpBody =
                    Json.Encode.object
                        [ ( "query", Json.Encode.string ("query{" ++ query ++ "}") )
                        ]
            in
            Http.task
                { method = "POST"
                , headers = []
                , url = apiEndpoint
                , body = Http.jsonBody httpBody
                , resolver = Http.stringResolver API.httpStringBodyResolver
                , timeout = Just 10000
                }


type Selection
    = SelectionLeaf { type_ : GraphQL.Type, description : Maybe String, selected : Bool }
    | SelectionNest SelectionNestAttrs
    | SelectionPending { type_ : GraphQL.Type, field : GraphQL.Field, selected : Bool }


type alias SelectionNestAttrs =
    { type_ : GraphQL.Type, field : GraphQL.Field, selected : Bool, inputValueDict : InputValueDict, selectionDict : SelectionDict }


type alias SelectionDict =
    Dict String Selection


type alias InputValueDict =
    Dict String InputValue


type InputValue
    = InputLeaf { type_ : GraphQL.Type, description : Maybe String, value : Maybe String }
    | InputNest { type_ : GraphQL.Type, description : Maybe String, selected : Bool } InputValueDict
    | InputUnion (List InputValue)
    | InputEnum { type_ : GraphQL.Type, description : Maybe String, value : Maybe String, options : List String }
    | InputList (List InputValue)


formQuery : String -> SelectionNestAttrs -> String
formQuery key form =
    let
        queries =
            Dict.map inputValueQuery form.inputValueDict
                |> Dict.values
                |> listWithoutNothing

        selectionInput =
            case queries of
                [] ->
                    key

                _ ->
                    key ++ "(" ++ String.join " " queries ++ ")"
    in
    selectionInput ++ "{" ++ selectionDictQuery form.selectionDict ++ "}"


selectionDictQuery : SelectionDict -> String
selectionDictQuery dict =
    Dict.map selectionQuery dict
        |> Dict.values
        |> listWithoutNothing
        |> String.join " "


selectionQuery : String -> Selection -> Maybe String
selectionQuery key selection =
    case selection of
        SelectionLeaf record ->
            if record.selected then
                Just key

            else
                Nothing

        SelectionNest record ->
            if record.selected then
                Just (formQuery record.field.name record)

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


renderForm : (String -> Maybe GraphQL.Type) -> List String -> String -> SelectionNestAttrs -> Html Msg
renderForm typeLookup keys key form =
    let
        bodyFooter =
            [ if form.inputValueDict == Dict.empty then
                text ""

              else
                div [ class "card-body" ] (Dict.values (Dict.map (renderInputValue (List.append keys [ key ])) form.inputValueDict))
            , div [ class "card-footer" ]
                [ div [ class "form-group form-check" ] (Dict.values (Dict.map (chooseSelection typeLookup (List.append keys [ key ])) form.selectionDict))
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


chooseSelection : (String -> Maybe GraphQL.Type) -> List String -> String -> Selection -> Html Msg
chooseSelection typeLookup keys key selection =
    let
        fieldName =
            String.join "-" keys ++ "-" ++ key
    in
    case selection of
        SelectionLeaf record ->
            div []
                [ input
                    [ type_ "checkbox"
                    , id fieldName
                    , class "form-check-input"
                    , checked record.selected
                    , value (boolMap record.selected "false" "true") -- opposite
                    , onInput (ModelChanged (toggleModelSelection typeLookup (List.append keys [ key ])))
                    ]
                    []
                , label [ for fieldName ]
                    [ text key
                    , div [] [ htmlDescription record.description ]
                    ]
                ]

        SelectionNest record ->
            div []
                [ input
                    [ type_ "checkbox"
                    , id fieldName
                    , class "form-check-input"
                    , checked record.selected
                    , value (boolMap record.selected "false" "true") -- opposite
                    , onInput (ModelChanged (toggleModelSelection typeLookup (List.append keys [ key ])))
                    ]
                    []
                , label [ for fieldName ] [ text key ]
                , renderForm typeLookup keys key record
                ]

        SelectionPending record ->
            div [ class "text-muted d-block" ]
                [ input
                    [ type_ "checkbox"
                    , id fieldName
                    , class "form-check-input"
                    , checked False
                    , value "true" -- opposite
                    , onInput (ModelChanged (toggleModelSelection typeLookup (List.append keys [ key ])))
                    ]
                    []
                , label [ for fieldName ]
                    [ text key
                    , div [] [ htmlDescription record.field.description ]
                    ]
                ]


toggleModelSelection : (String -> Maybe GraphQL.Type) -> List String -> Model -> String -> Model
toggleModelSelection typeLookup keys model value =
    case model.selection of
        Just (SelectionNest selection) ->
            { model | selection = Just (toggleSelection typeLookup (List.drop 1 keys) (SelectionNest selection)) }

        _ ->
            model


toggleSelection : (String -> Maybe GraphQL.Type) -> List String -> Selection -> Selection
toggleSelection typeLookup keys selection =
    case selection of
        SelectionLeaf record ->
            SelectionLeaf { record | selected = not record.selected }

        SelectionNest record ->
            case keys of
                [] ->
                    if record.selected then
                        SelectionPending { type_ = record.type_, field = record.field, selected = False }

                    else
                        SelectionNest { record | selected = not record.selected }

                x :: xs ->
                    SelectionNest { record | selectionDict = Dict.update x (Maybe.map (toggleSelection typeLookup xs)) record.selectionDict }

        SelectionPending record ->
            graphqlFieldToSelection
                typeLookup
                record.field.description
                record.field.name
                record.type_
                record.field
                True


setModelFormValue : List String -> Model -> String -> Model
setModelFormValue keys model value =
    case model.selection of
        Just (SelectionNest record) ->
            let
                newRecord =
                    setFormValue (List.drop 1 keys) record value
            in
            { model | selection = Just (SelectionNest newRecord) }

        _ ->
            model


setFormValue : List String -> SelectionNestAttrs -> String -> SelectionNestAttrs
setFormValue keys form value =
    case keys of
        x :: xs ->
            case Dict.get x form.inputValueDict of
                Just _ ->
                    { form | inputValueDict = Dict.update x (Maybe.map (setInputValue xs value)) form.inputValueDict }

                Nothing ->
                    { form | selectionDict = Dict.update x (Maybe.map (setSelectionValue xs value)) form.selectionDict }

        _ ->
            form


setSelectionValue : List String -> String -> Selection -> Selection
setSelectionValue keys value selection =
    let
        unexpectedSelection =
            let
                _ =
                    Debug.log "UNEXPECTED setSelectionValue" ( keys, value, selection )
            in
            selection
    in
    case selection of
        SelectionLeaf record ->
            unexpectedSelection

        SelectionNest record ->
            SelectionNest (setFormValue keys record value)

        SelectionPending record ->
            unexpectedSelection


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


graphqlFieldToForm : (String -> Maybe GraphQL.Type) -> GraphQL.Type -> GraphQL.Field -> Bool -> SelectionNestAttrs
graphqlFieldToForm typeLookup graphqlType graphqlField selected =
    { type_ = graphqlType
    , field = graphqlField
    , selected = selected
    , inputValueDict =
        List.foldl
            (\arg dict ->
                Dict.insert arg.name
                    (graphqlTypeToInputValue typeLookup Nothing arg.description (absoluteType typeLookup arg.type_))
                    dict
            )
            Dict.empty
            graphqlField.args
    , selectionDict =
        absoluteType typeLookup graphqlField.type_
            |> graphqlTypeToSelection typeLookup graphqlField.description graphqlField.name
    }


graphqlFieldToSelection : (String -> Maybe GraphQL.Type) -> Maybe String -> String -> GraphQL.Type -> GraphQL.Field -> Bool -> Selection
graphqlFieldToSelection typeLookup description name graphqlType graphqlField selected =
    if selected then
        -- SelectionNest
        --     { type_ = graphqlType, field = graphqlField, selected = selected }
        --     (graphqlFieldToForm typeLookup graphqlField graphqlField.description)
        case graphqlType of
            GraphQL.TypeScalar attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeObject attrs ->
                SelectionNest (graphqlFieldToForm typeLookup graphqlType graphqlField selected)

            GraphQL.TypeInterface attrs ->
                SelectionNest (graphqlFieldToForm typeLookup graphqlType graphqlField selected)

            GraphQL.TypeUnion attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeEnum attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeInput attrs ->
                SelectionLeaf { type_ = graphqlType, selected = selected, description = description }

            GraphQL.TypeNotNull attrs ->
                graphqlFieldToSelection typeLookup description name attrs.ofType graphqlField selected

            GraphQL.TypeList attrs ->
                graphqlFieldToSelection typeLookup description name attrs.ofType graphqlField selected

    else
        SelectionPending
            { type_ = graphqlType, field = graphqlField, selected = selected }


graphqlTypeToSelection : (String -> Maybe GraphQL.Type) -> Maybe String -> String -> GraphQL.Type -> SelectionDict
graphqlTypeToSelection typeLookup description name graphqlType =
    let
        -- given `attrs`, return a Dict of single key-value
        singleNameSelection attrs =
            Dict.fromList [ ( name, SelectionLeaf { type_ = graphqlType, selected = False, description = attrs.description } ) ]

        -- insert field+type into a dict
        insertNameSelection dict field type_ =
            Dict.insert
                field.name
                (graphqlFieldToSelection typeLookup description name type_ field False)
                dict

        -- fold a `List Field` into `SelectionDict`
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
                |> graphqlTypeToSelection typeLookup description name

        GraphQL.TypeList attrs ->
            absoluteType typeLookup attrs.ofType
                |> graphqlTypeToSelection typeLookup description name


graphqlTypeToInputValue : (String -> Maybe GraphQL.Type) -> Maybe String -> Maybe String -> GraphQL.Type -> InputValue
graphqlTypeToInputValue typeLookup value description graphqlType =
    case graphqlType of
        GraphQL.TypeScalar attrs ->
            InputLeaf { description = description, type_ = graphqlType, value = value }

        GraphQL.TypeObject attrs ->
            -- TODO: graphqlType.interfaces : Maybe (List Type)
            InputNest
                { description = description, type_ = graphqlType, selected = maybeSomething value }
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
