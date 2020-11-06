module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import API
import Array exposing (Array)
import Base64
import Browser
import Browser.Events exposing (onClick)
import Browser.Navigation
import Dev
import Dict exposing (Dict)
import GraphQL exposing (typeName)
import GraphQL.Form
import Html exposing (Html, a, br, button, code, div, em, footer, form, h1, h2, h3, h5, hr, img, input, label, li, main_, nav, node, option, p, pre, select, small, span, strong, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, for, href, id, name, placeholder, rel, required, src, style, target, title, type_, value)
import Html.Events exposing (on, onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Json.Encode
import List exposing (sum)
import MaybeExtra exposing (boolMap, listWithoutNothing, maybeSomething, nothingIfBlank)
import Regex
import RemoteData
import Route
import Task exposing (Task)
import Templates
import Time
import UI
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


type alias Model =
    { navKey : Browser.Navigation.Key
    , route : Route.Route
    , alert : Maybe UI.Alert
    , schemaLookup : Dict String GraphQL.Schema
    , schema : RemoteData.WebData GraphQL.Schema
    , types : Dict String GraphQL.Type
    , type_ : Maybe GraphQL.Type
    , field : Maybe GraphQL.Field
    , displayQuery : Bool
    , selectionKey : String
    , selection : RemoteData.WebData Selection
    , dstyle : DictRenderStyle
    , graphqlResponseResult : RemoteData.WebData GraphQLResponse
    , headersLookup : Dict String String
    , apiURL : String
    , apiHeaders : Maybe String
    , corsEscapePrefix : String
    }


type alias GraphQLResponse =
    { data : Maybe DictValue
    , errors : Maybe (List { message : String })
    }


decodeGraphQLResponse : Json.Decode.Decoder GraphQLResponse
decodeGraphQLResponse =
    Json.Decode.map2 GraphQLResponse
        (Json.Decode.maybe (Json.Decode.field "data" jsonDecodeDictValue))
        (Json.Decode.maybe
            (Json.Decode.field "errors"
                (Json.Decode.list
                    (Json.Decode.map (\s -> { message = s })
                        (Json.Decode.field "message" Json.Decode.string)
                    )
                )
            )
        )


type alias Flags =
    Maybe
        { apiURL : String
        , apiHeaders : String
        , corsEscapePrefix : String
        }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | ModelChanged (Model -> String -> Model) String
    | OnIntrospectionResponse String (Result Http.Error String)
    | ChosenSchema String GraphQL.Field
    | ApiUrlUpdated
    | FormSubmitted
    | OnFormResponse (Result Http.Error GraphQLResponse)
    | ChosenDictRenderStyle DictRenderStyle


type DictValue
    = NullValue
    | StringValue String
    | BoolValue Bool
    | IntValue Int
    | FloatValue Float
    | ListValue (List DictValue)
    | NestedValue (Dict String DictValue)


jsonDecodeDictValue : Json.Decode.Decoder DictValue
jsonDecodeDictValue =
    Json.Decode.oneOf
        [ Json.Decode.null NullValue
        , Json.Decode.map StringValue Json.Decode.string
        , Json.Decode.map BoolValue Json.Decode.bool
        , Json.Decode.map IntValue Json.Decode.int
        , Json.Decode.map FloatValue Json.Decode.float
        , Json.Decode.map ListValue (Json.Decode.lazy (\_ -> Json.Decode.list jsonDecodeDictValue))
        , Json.Decode.map NestedValue (Json.Decode.lazy (\_ -> Json.Decode.dict jsonDecodeDictValue))
        ]


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        flagApi =
            flags
                |> Maybe.map (\f -> Dict.fromList [ ( f.apiURL, f.apiHeaders ) ])
                |> Maybe.withDefault Dict.empty

        model =
            { navKey = navKey
            , route = Route.fromUrl url
            , alert = Nothing
            , schemaLookup = Dict.empty
            , schema = RemoteData.NotAsked
            , types = Dict.empty
            , type_ = Nothing
            , field = Nothing
            , displayQuery = False
            , selectionKey = ""
            , selection = RemoteData.NotAsked
            , dstyle = DictAsCard
            , graphqlResponseResult = RemoteData.NotAsked
            , headersLookup = flagApi
            , apiURL = ""
            , apiHeaders = Nothing
            , corsEscapePrefix = Maybe.withDefault "https://cors-anywhere.herokuapp.com/" (Maybe.map .corsEscapePrefix flags)
            }
    in
    updateRoute model.route model


routeBreadCrumb : Route.Route -> Html Msg
routeBreadCrumb routeRoute =
    case routeRoute of
        Route.NotFound ->
            UI.breadcrumbs [] [] "Not found"

        Route.Homepage _ ->
            UI.breadcrumbs [] [] "Home"

        Route.OperationTypes apiURL ->
            UI.breadcrumbs [] [ "Home" ] apiURL

        Route.SelectionSets apiURL operationType ->
            UI.breadcrumbs [] [ "Home", apiURL ] operationType

        Route.Request apiURL operationType selectionSet ->
            UI.breadcrumbs [] [ "Home", apiURL, operationType ] selectionSet


view : Model -> Browser.Document Msg
view model =
    Browser.Document "App"
        [ node "link"
            [ rel "stylesheet"
            , href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
            ]
            []
        , node "meta" [ name "viewport", attribute "content" "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no" ] []
        , UI.alert model.alert
        , case model.route of
            Route.NotFound ->
                main_ [ class "container" ]
                    [ div [ class "jumbotron mt-3" ]
                        [ h1 [] [ a [ href "/" ] [ text "GraphQL", span [ class "text-muted" ] [ text ".html" ] ] ]
                        , p [ class "lead" ] [ text "Given any GraphQL endpoint, render an HTML form" ]
                        ]
                    , div [ class "mt-3", title (Debug.toString model.route) ]
                        [ routeBreadCrumb model.route ]
                    , a [ href "/" ] [ text "Go back" ]
                    ]

            Route.Homepage _ ->
                main_ [ class "container" ]
                    [ div [ class "jumbotron mt-3" ]
                        [ h1 [] [ a [ href "/" ] [ text "GraphQL", span [ class "text-muted" ] [ text ".html" ] ] ]
                        , p [ class "lead" ] [ text "Given any GraphQL endpoint, render an HTML form" ]
                        ]
                    , endpointForm model
                    ]

            Route.OperationTypes apiURL ->
                main_ [ class "container" ]
                    [ div [ class "jumbotron mt-3" ]
                        [ h1 [] [ a [ href "/" ] [ text "GraphQL", span [ class "text-muted" ] [ text ".html" ] ] ]
                        , p [ class "lead" ] [ text "Given any GraphQL endpoint, render an HTML form" ]
                        ]
                    , div [ class "mt-3", title (Debug.toString model.route) ]
                        [ routeBreadCrumb model.route ]
                    , renderRemote "Introspecting endpoint..." (renderGraphqlSchema apiURL Nothing model.types) model.schema
                    ]

            Route.SelectionSets apiURL operationType ->
                main_ [ class "container" ]
                    [ div [ class "jumbotron mt-3" ]
                        [ h1 [] [ a [ href "/" ] [ text "GraphQL", span [ class "text-muted" ] [ text ".html" ] ] ]
                        , p [ class "lead" ] [ text "Given any GraphQL endpoint, render an HTML form" ]
                        ]
                    , div [ class "mt-3", title (Debug.toString model.route) ]
                        [ routeBreadCrumb model.route ]
                    , renderRemote "Introspecting endpoint..." (renderGraphqlSchema apiURL (Just operationType) model.types) model.schema
                    ]

            Route.Request apiURL operationType selectionSet ->
                main_ [ class "container" ]
                    [ div [ class "jumbotron mt-3" ]
                        [ h1 [] [ a [ href "/" ] [ text "GraphQL", span [ class "text-muted" ] [ text ".html" ] ] ]
                        , p [ class "lead" ] [ text "Given any GraphQL endpoint, render an HTML form" ]
                        ]
                    , div [ class "mt-3", title (Debug.toString model.route) ]
                        [ routeBreadCrumb model.route ]
                    , renderRemote "Introspecting endpoint..."
                        (always
                            (div []
                                [ renderRemote "Introspecting endpoint..." (renderSelectionForm (\s -> Dict.get s model.types) model) model.selection
                                , renderRemote "Waiting for reply..." (renderGraphqlResponse model.dstyle) model.graphqlResponseResult
                                ]
                            )
                        )
                        model.schema
                    ]
        , footer [ class "container mt-3 mb-3 text-right" ]
            [ a [ href "https://github.com/choonkeat/GraphQL.html", target "_blank" ]
                [ small [] [ text "github.com/choonkeat/GraphQL.html" ] ]
            ]
        ]


endpointForm : Model -> Html Msg
endpointForm model =
    let
        exemplify string =
            a [ href ("/" ++ Base64.encode string ++ "/") ] [ text string ]
    in
    form [ onSubmit ApiUrlUpdated ]
        [ UI.inputString
            { label = [ text "GraphQL Endpoint" ]
            , htmlType = "text"
            , value = model.apiURL
            , description =
                small [ class "text-muted" ]
                    (List.concat
                        [ [ text "e.g. " ]
                        , List.intersperse (text ", ")
                            (List.map exemplify
                                [ "https://graphql.anilist.co/"
                                ]
                            )
                        , [ text "; see "
                          , a [ href "http://apis.guru/graphql-apis/", target "_blank" ] [ text "http://apis.guru/graphql-apis/" ]
                          , text " for more APIs."
                          ]
                        ]
                    )
            , attrs =
                [ onInput (ModelChanged (\m s -> { m | apiURL = s }))
                ]
            }
        , case model.apiHeaders of
            Just apiHeaders ->
                div []
                    [ UI.inputText
                        { label = [ text "HTTP Request Headers" ]
                        , value = apiHeaders
                        , description = UI.description (Just "e.g. Authorization: Bearer abc1234")
                        , attrs =
                            [ onInput (ModelChanged (\m s -> { m | apiHeaders = Just s }))
                            , placeholder "optional"
                            , style "height" "3em"
                            ]
                        }
                    , UI.inputString
                        { label = [ text "CORS Proxy" ]
                        , htmlType = "text"
                        , value = model.corsEscapePrefix
                        , description = UI.description (Just "use a reverse proxy to workaround CORS restriction")
                        , attrs = [ onInput (ModelChanged (\m s -> { m | corsEscapePrefix = s })) ]
                        }
                    ]

            Nothing ->
                UI.inputCheckbox
                    { label = [ text "advanced settings" ]
                    , description = UI.description (Just "e.g. set headers, workaround CORS...")
                    , attrs = [ onInput (ModelChanged (\m s -> { m | apiHeaders = Just "" })) ]
                    }
        , hr [] []
        , UI.submitButton { loading = False }
        ]


renderGraphqlSchema : String -> Maybe String -> Dict String GraphQL.Type -> GraphQL.Schema -> Html Msg
renderGraphqlSchema apiURL maybeOperation types schemaGraphQL =
    let
        operationTypes =
            [ ( "query", Just schemaGraphQL.queryType )
            , ( "mutation", schemaGraphQL.mutationType )
            , ( "subscription", schemaGraphQL.subscriptionType )
            ]
                |> List.filter (\( k, _ ) -> Maybe.withDefault k maybeOperation == k)

        renderCard ( heading, maybeHeadingType ) =
            case maybeHeadingType of
                Nothing ->
                    text ""

                Just headingType ->
                    div [ class "card mb-3" ]
                        [ h5 [ class "card-header", style "text-transform" "capitalize" ]
                            [ text heading ]
                        , viewQueriesNav ("/" ++ Base64.encode apiURL ++ "/" ++ heading) (GraphQL.fields types headingType)
                        ]
    in
    div [] (List.map renderCard operationTypes)


fieldsOf : Dict String GraphQL.Type -> GraphQL.Schema -> String -> Maybe (List GraphQL.Field)
fieldsOf types schemaGraphQL operationName =
    Maybe.map (GraphQL.fields types)
        (case operationName of
            "mutation" ->
                schemaGraphQL.mutationType

            "subscription" ->
                schemaGraphQL.subscriptionType

            "query" ->
                Just schemaGraphQL.queryType

            _ ->
                Nothing
        )


renderSelectionForm : (String -> Maybe GraphQL.Type) -> Model -> Selection -> Html Msg
renderSelectionForm typeLookup model selection =
    case selection of
        SelectionNest record ->
            renderSelectionNestForm typeLookup model record

        _ ->
            text ""


renderSelectionNestForm : (String -> Maybe GraphQL.Type) -> Model -> SelectionNestAttrs -> Html Msg
renderSelectionNestForm typeLookup model record =
    div [ class (boolMap model.displayQuery "row" "") ]
        [ form [ onSubmit FormSubmitted, class (boolMap model.displayQuery "col-md-6 mb-3" "mb-3") ]
            [ renderForm typeLookup [] record.field.name record
            , div [ class "mb-3" ]
                [ UI.inputCheckbox
                    { label = [ text "Show GraphQL query" ]
                    , description = text ""
                    , attrs =
                        [ checked model.displayQuery
                        , onInput (ModelChanged (\m s -> { m | displayQuery = not m.displayQuery }))
                        ]
                    }
                ]
            , div [ class "mb-3" ]
                [ label [] [ text "Display response" ]
                , UI.inputCheckbox
                    { label = [ text "as cards" ]
                    , description = text ""
                    , attrs =
                        [ type_ "radio"
                        , name "ChosenDictRenderStyle"
                        , checked (model.dstyle == DictAsCard)
                        , onClick (ChosenDictRenderStyle DictAsCard)
                        ]
                    }
                , UI.inputCheckbox
                    { label = [ text "as table rows" ]
                    , description = text ""
                    , attrs =
                        [ type_ "radio"
                        , name "ChosenDictRenderStyle"
                        , checked (model.dstyle == DictAsTable)
                        , onClick (ChosenDictRenderStyle DictAsTable)
                        ]
                    }
                ]
            , case model.graphqlResponseResult of
                RemoteData.Loading ->
                    UI.submitButton { loading = True }

                _ ->
                    UI.submitButton { loading = False }
            ]
        , maybeRender
            (UI.codeBlock { attrs = [ class "p-3 col-md-6" ] })
            (boolMap model.displayQuery (Just (formQuery record.field.name record)) Nothing)
        ]


renderGraphqlResponse : DictRenderStyle -> GraphQLResponse -> Html Msg
renderGraphqlResponse dstyle graphQLResponse =
    div []
        [ case graphQLResponse.errors of
            Just (x :: xs) ->
                div []
                    (x
                        :: xs
                        |> List.map (\err -> UI.alert (Just { category = "danger", message = err.message }))
                    )

            _ ->
                text ""
        , case graphQLResponse.data of
            Just dictValue ->
                renderDictValue dstyle dictValue

            Nothing ->
                text ""
        ]


type DictRenderStyle
    = DictAsTable
    | DictAsCard


renderDictAsTable : List String -> List DictValue -> Html Msg
renderDictAsTable keys rows =
    table [ class "table table-borderless" ]
        [ thead [ class "" ] [ tr [] (List.map (\k -> th [ class "col", style "text-transform" "capitalize" ] [ text k ]) keys) ]
        , tbody []
            (List.map
                (\row ->
                    case row of
                        NestedValue d ->
                            tr [] (Dict.values (Dict.map (\k v -> td [] [ renderDictValue DictAsTable v ]) d))

                        _ ->
                            text ""
                )
                rows
            )
        ]


renderDictAsCard : List DictValue -> Html Msg
renderDictAsCard rows =
    let
        listGroup k v =
            li [ class "list-group-item" ]
                [ div [] [ small [ class "text-muted", style "text-transform" "capitalize" ] [ text k ] ]
                , div [ class "row-fluid" ] [ renderDictValue DictAsCard v ]
                ]
    in
    div []
        (List.map
            (\row ->
                case row of
                    NestedValue dict ->
                        div [ class "list-group mb-3" ] (Dict.values (Dict.map listGroup dict))

                    _ ->
                        text ""
            )
            rows
        )


renderDictValue : DictRenderStyle -> DictValue -> Html Msg
renderDictValue dstyle dv =
    let
        raw s =
            div []
                [ text s ]
    in
    case dv of
        NullValue ->
            raw "null"

        StringValue string ->
            if List.foldl (\ext sum -> sum || String.endsWith ext string) False [ ".jpg", ".png", ".gif", ".webp" ] then
                case Url.fromString string of
                    Just _ ->
                        -- sample of rich data presentation
                        img [ src string, class "img-fluid img-thumbnail" ] []

                    Nothing ->
                        raw string

            else
                raw string

        BoolValue True ->
            raw "true"

        BoolValue False ->
            raw "false"

        IntValue int ->
            raw (String.fromInt int)

        FloatValue float ->
            raw (String.fromFloat float)

        ListValue (x :: xs) ->
            case x of
                NestedValue dict ->
                    case dstyle of
                        DictAsTable ->
                            renderDictAsTable (Dict.keys dict) (x :: xs)

                        DictAsCard ->
                            renderDictAsCard (x :: xs)

                _ ->
                    div [ class "list-group" ]
                        (List.map (\v -> li [ class "list-group-item" ] [ renderDictValue dstyle v ]) (x :: xs))

        ListValue [] ->
            em [] [ text "no data" ]

        NestedValue dict ->
            div [ class "list-group" ]
                (Dict.map
                    (\k v ->
                        li [ class "list-group-item" ]
                            [ div [] [ small [ class "text-muted", style "text-transform" "capitalize" ] [ text k ] ]
                            , div [ class "row-fluid" ] [ renderDictValue dstyle v ]
                            ]
                    )
                    dict
                    |> Dict.values
                )


renderRemote : String -> (a -> Html Msg) -> RemoteData.WebData a -> Html Msg
renderRemote loadingMessage render webData =
    case webData of
        RemoteData.NotAsked ->
            div [ style "min-height" "500px" ] [ text "" ]

        RemoteData.Loading ->
            div [ style "min-height" "500px" ]
                [ UI.progress { category = "primary", message = loadingMessage } 100.0 ]

        RemoteData.Failure err ->
            div [ style "min-height" "500px" ]
                [ UI.alert (Just { category = "danger", message = Debug.toString err }) ]

        RemoteData.Success data ->
            render data


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


viewQueriesNav : String -> List GraphQL.Field -> Html Msg
viewQueriesNav pathPrefix list =
    div [ class "list-group-flush", style "margin-top" "-1px" ]
        (List.map (viewSchemaType pathPrefix) list)


viewSchemaType : String -> GraphQL.Field -> Html Msg
viewSchemaType pathPrefix field =
    a
        [ class "list-group-item list-group-item-action"
        , href (pathPrefix ++ "/" ++ field.name ++ "/")
        ]
        [ h5 [ class "mb-1" ] [ text field.name ]
        , p [ class "mb-1" ] [ text (Maybe.withDefault "" field.description) ]
        ]


updateRoute : Route.Route -> Model -> ( Model, Cmd Msg )
updateRoute routeRoute model =
    let
        modelWithSchema apiURL =
            case ( Dict.get apiURL model.headersLookup, Dict.get apiURL model.schemaLookup ) of
                ( _, Just schema ) ->
                    ( { model | schema = RemoteData.Success schema }, Cmd.none )

                ( Just headers, Nothing ) ->
                    ( { model | alert = Nothing, schema = RemoteData.Loading, apiURL = apiURL, apiHeaders = Just headers }
                    , Task.attempt (OnIntrospectionResponse apiURL) (API.introspect (Debug.log "introspecting" apiURL))
                    )

                _ ->
                    ( { model | alert = Nothing, schema = RemoteData.Loading, apiURL = apiURL, apiHeaders = Nothing }
                    , Task.attempt (OnIntrospectionResponse apiURL) (API.introspect (Debug.log "introspecting" apiURL))
                    )
    in
    case routeRoute of
        Route.NotFound ->
            ( model, Cmd.none )

        Route.Homepage query ->
            case ( query.p, query.q ) of
                ( Nothing, _ ) ->
                    ( { model | alert = Nothing, schema = RemoteData.NotAsked }, Cmd.none )

                ( Just oldpath, Nothing ) ->
                    ( model, Browser.Navigation.replaceUrl model.navKey oldpath )

                ( Just oldpath, Just oldquery ) ->
                    ( model, Browser.Navigation.replaceUrl model.navKey (oldpath ++ "?" ++ oldquery) )

        Route.OperationTypes apiURL ->
            modelWithSchema apiURL

        Route.SelectionSets apiURL operationType ->
            let
                ( newModel, newCmd ) =
                    modelWithSchema apiURL
            in
            ( newModel, newCmd )

        Route.Request apiURL operationType selectionSet ->
            let
                ( newModel, newCmd ) =
                    modelWithSchema apiURL
            in
            case newModel.schema of
                RemoteData.NotAsked ->
                    ( newModel, newCmd )

                RemoteData.Loading ->
                    ( newModel, newCmd )

                RemoteData.Failure err ->
                    ( { newModel | alert = Just { category = "danger", message = Debug.toString err } }, newCmd )

                RemoteData.Success schema ->
                    case fieldsOf newModel.types schema operationType of
                        Nothing ->
                            ( { newModel | alert = Just { category = "danger", message = "fields of found: " ++ operationType } }, Cmd.none )

                        Just fields ->
                            case
                                List.foldl
                                    (\field sum ->
                                        if field.name == selectionSet then
                                            Just field

                                        else
                                            sum
                                    )
                                    Nothing
                                    fields
                            of
                                Nothing ->
                                    ( { newModel | alert = Just { category = "danger", message = "Field not found: " ++ selectionSet } }, Cmd.none )

                                Just field ->
                                    let
                                        typeLookup s =
                                            Dict.get s newModel.types

                                        newType =
                                            GraphQL.fieldType newModel.types field

                                        newField =
                                            Just field

                                        newSelection =
                                            case newType of
                                                Nothing ->
                                                    newModel.selection

                                                Just t ->
                                                    RemoteData.Success (SelectionNest (graphqlFieldToForm typeLookup t field True))
                                    in
                                    ( { newModel
                                        | type_ = newType
                                        , field = newField
                                        , selectionKey = operationType
                                        , selection = newSelection
                                        , graphqlResponseResult = RemoteData.NotAsked
                                        , alert = Nothing
                                      }
                                    , Cmd.none
                                    )


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
            let
                newRoute =
                    Route.fromUrl urlUrl
            in
            updateRoute newRoute { model | route = newRoute }

        ModelChanged function string ->
            ( function model string, Cmd.none )

        OnIntrospectionResponse apiURL (Ok string) ->
            case Json.Decode.decodeString GraphQL.decodeIntrospectResponse string of
                Err oops ->
                    ( { model | alert = Just { category = "danger", message = Json.Decode.errorToString oops }, selection = RemoteData.NotAsked }, Cmd.none )

                Ok resp ->
                    let
                        newTypes =
                            GraphQL.typesDict resp.data.schema.types Dict.empty

                        newSchemaLookup =
                            Dict.update apiURL (\v -> Just resp.data.schema) model.schemaLookup
                    in
                    updateRoute model.route
                        { model
                            | alert = Nothing
                            , schemaLookup = newSchemaLookup
                            , schema = RemoteData.Success resp.data.schema
                            , types = newTypes
                            , selection = RemoteData.NotAsked
                        }

        OnIntrospectionResponse apiURL (Err Http.NetworkError) ->
            let
                newApiURL =
                    model.corsEscapePrefix ++ apiURL
            in
            if String.startsWith model.corsEscapePrefix apiURL then
                ( { model | alert = Just { category = "danger", message = "NetworkError" }, selection = RemoteData.NotAsked }, Cmd.none )

            else
                update ApiUrlUpdated { model | apiURL = newApiURL }

        OnIntrospectionResponse apiURL (Err err) ->
            ( { model | alert = Just { category = "danger", message = Debug.toString err }, selection = RemoteData.NotAsked }, Cmd.none )

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
                            RemoteData.Success (SelectionNest (graphqlFieldToForm typeLookup t field True))
            in
            ( { model
                | type_ = newType
                , field = newField
                , selectionKey = heading
                , selection = newSelection
                , graphqlResponseResult = RemoteData.NotAsked
                , alert = Nothing
              }
            , Cmd.none
            )

        ApiUrlUpdated ->
            let
                shortName =
                    Base64.encode model.apiURL

                newHeadersLookup =
                    Dict.insert model.apiURL (Maybe.withDefault "" model.apiHeaders) model.headersLookup
            in
            ( { model | alert = Nothing, selection = RemoteData.Loading, headersLookup = newHeadersLookup }
            , Browser.Navigation.pushUrl model.navKey ("/" ++ shortName ++ "/")
            )

        FormSubmitted ->
            case model.selection of
                RemoteData.Success selection ->
                    ( { model | graphqlResponseResult = RemoteData.Loading, alert = Nothing }
                    , httpRequest model.apiURL (textareaToHttpHeaders (Maybe.withDefault "" model.apiHeaders)) model.selectionKey selection
                        |> Task.attempt OnFormResponse
                    )

                _ ->
                    ( { model | alert = Nothing }, Cmd.none )

        OnFormResponse (Ok data) ->
            ( { model | graphqlResponseResult = RemoteData.fromResult (Ok data), alert = Nothing }, Cmd.none )

        OnFormResponse (Err err) ->
            ( { model | alert = Just { category = "danger", message = Debug.toString err }, graphqlResponseResult = RemoteData.Failure err }, Cmd.none )

        ChosenDictRenderStyle dstyle ->
            ( { model | dstyle = dstyle, alert = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


textareaToHttpHeaders : String -> List Http.Header
textareaToHttpHeaders string =
    let
        delimiter =
            ": "
    in
    String.split "\n" (String.trim string)
        |> List.foldl
            (\kv sum ->
                case String.split delimiter (String.trim kv) of
                    "" :: xs ->
                        sum

                    k :: vs ->
                        List.append sum [ Http.header k (String.join delimiter vs) ]

                    _ ->
                        sum
            )
            []


httpRequest : String -> List Http.Header -> String -> Selection -> Task Http.Error GraphQLResponse
httpRequest apiEndpoint headerKeyValues selectionKey selection =
    case selectionQuery "" selection of
        Nothing ->
            Task.fail (Http.BadBody "Incomplete request form")

        Just query ->
            let
                httpBody =
                    Json.Encode.object
                        [ ( "query", Json.Encode.string (selectionKey ++ "{" ++ query ++ "}") )
                        ]
            in
            Http.task
                { method = "POST"
                , headers = headerKeyValues
                , url = apiEndpoint
                , body = Http.jsonBody httpBody
                , resolver = Http.stringResolver (API.httpJsonBodyResolver decodeGraphQLResponse)
                , timeout = Just 10000
                }


type Selection
    = SelectionLeaf { type_ : GraphQL.Type, description : Maybe String, selected : Bool }
    | SelectionNest SelectionNestAttrs
    | SelectionPending { type_ : GraphQL.Type, field : GraphQL.Field, selected : Bool }


type alias SelectionNestAttrs =
    { type_ : GraphQL.Type, field : GraphQL.Field, selected : Bool, inputValueDict : Dict String GraphQL.Form.Value, selectionDict : SelectionDict }


type alias SelectionDict =
    Dict String Selection


formQuery : String -> SelectionNestAttrs -> String
formQuery key form =
    let
        namedArgs =
            Dict.map GraphQL.Form.valueToNamedArgs form.inputValueDict
                |> Dict.values
                |> listWithoutNothing

        selectionInput =
            if List.isEmpty namedArgs then
                key

            else
                key ++ "(" ++ String.join " " namedArgs ++ ")"

        subselect =
            selectionDictQuery form.selectionDict
    in
    if subselect == "" then
        -- this `form` is effectively NOT selected since
        -- no sub selections are made; otherwise invalid graphql
        ""

    else
        selectionInput ++ "{" ++ subselect ++ "}"


selectionDictQuery : SelectionDict -> String
selectionDictQuery dict =
    Dict.map selectionQuery dict
        |> Dict.values
        |> List.map nothingIfBlank
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



-- Just (String.join ", " (List.map (inputValueQuery key) inputValueList))


renderForm : (String -> Maybe GraphQL.Type) -> List String -> String -> SelectionNestAttrs -> Html Msg
renderForm typeLookup keys key form =
    let
        inputForm k v =
            GraphQL.Form.valueToHTML
                (ModelChanged (setModelFormValue (List.append keys [ key, k ])))
                (List.append keys [ key ])
                k
                v

        bodyFooter =
            [ if form.inputValueDict == Dict.empty then
                text ""

              else
                div [ class "card-body" ] (Dict.values (Dict.map inputForm form.inputValueDict))
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
            UI.inputCheckbox
                { label = [ text key ]
                , description = UI.description record.description
                , attrs =
                    [ checked record.selected
                    , value (boolMap record.selected "false" "true") -- opposite
                    , onInput (ModelChanged (toggleModelSelection typeLookup (List.append keys [ key ])))
                    ]
                }

        SelectionNest record ->
            div []
                [ UI.inputCheckbox
                    { label = [ text key ]
                    , description = text ""
                    , attrs =
                        [ checked record.selected
                        , value (boolMap record.selected "false" "true") -- opposite
                        , onInput (ModelChanged (toggleModelSelection typeLookup (List.append keys [ key ])))
                        ]
                    }
                , renderForm typeLookup keys key record
                ]

        SelectionPending record ->
            let
                label =
                    if GraphQL.isComplicated record.field.type_ then
                        [ text (key ++ " ")
                        , span [ class "ml-2 badge badge-secondary" ] [ text "..." ]
                        ]

                    else
                        [ text key ]
            in
            div [ class "text-muted d-block" ]
                [ UI.inputCheckbox
                    { label = label
                    , description = UI.description record.field.description
                    , attrs =
                        [ checked False
                        , value "true" -- opposite
                        , onInput (ModelChanged (toggleModelSelection typeLookup (List.append keys [ key ])))
                        ]
                    }
                ]


toggleModelSelection : (String -> Maybe GraphQL.Type) -> List String -> Model -> String -> Model
toggleModelSelection typeLookup keys model value =
    case model.selection of
        RemoteData.Success (SelectionNest selection) ->
            { model | selection = RemoteData.Success (toggleSelection typeLookup (List.drop 1 keys) (SelectionNest selection)) }

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
        RemoteData.Success (SelectionNest record) ->
            let
                newRecord =
                    setFormValue (List.drop 1 keys) record value
            in
            { model | selection = RemoteData.Success (SelectionNest newRecord) }

        _ ->
            model


setFormValue : List String -> SelectionNestAttrs -> String -> SelectionNestAttrs
setFormValue keys form value =
    case keys of
        x :: xs ->
            case Dict.get x form.inputValueDict of
                Just _ ->
                    { form | inputValueDict = Dict.update x (Maybe.map (GraphQL.Form.valueFromString xs value)) form.inputValueDict }

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


graphqlFieldToForm : (String -> Maybe GraphQL.Type) -> GraphQL.Type -> GraphQL.Field -> Bool -> SelectionNestAttrs
graphqlFieldToForm typeLookup graphqlType graphqlField selected =
    { type_ = graphqlType
    , field = graphqlField
    , selected = selected
    , inputValueDict =
        List.foldl
            (\arg dict ->
                Dict.insert arg.name
                    (GraphQL.Form.valueFromGraphqlType typeLookup Nothing arg.description (GraphQL.expandedType typeLookup arg.type_))
                    dict
            )
            Dict.empty
            graphqlField.args
    , selectionDict =
        GraphQL.expandedType typeLookup graphqlField.type_
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
                -- TODO: unsupported
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
                    GraphQL.expandedType typeLookup field.type_
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
            -- TODO: unsupported
            singleNameSelection attrs

        GraphQL.TypeEnum attrs ->
            singleNameSelection attrs

        GraphQL.TypeInput attrs ->
            singleNameSelection attrs

        GraphQL.TypeNotNull attrs ->
            GraphQL.expandedType typeLookup attrs.ofType
                |> graphqlTypeToSelection typeLookup description name

        GraphQL.TypeList attrs ->
            GraphQL.expandedType typeLookup attrs.ofType
                |> graphqlTypeToSelection typeLookup description name


maybeRender : (a -> Html b) -> Maybe a -> Html b
maybeRender function aMaybe =
    Maybe.map function aMaybe
        |> Maybe.withDefault (text "")
