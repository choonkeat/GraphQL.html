module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import API
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h1, h3, h5, input, label, main_, nav, node, p, pre, small, text, ul)
import Html.Attributes exposing (class, for, href, id, rel, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Task exposing (Task)
import Time
import Types
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
    , schema : Types.DataSchema
    , types : Dict String Types.SchemaType
    , schemaType : Maybe Types.SchemaType
    , typeField : Maybe Types.TypeField
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
    | OnHttpResponse (Result Http.Error Types.DataSchema)
    | ChosenSchema ( Types.TypeField, Types.SchemaType )
    | ApiUrlUpdated


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { navKey = navKey
            , alert = Nothing
            , schema = { mutationType = Nothing, queryType = Nothing, types = [] }
            , types = Dict.empty
            , schemaType = Nothing
            , typeField = Nothing

            -- , apiURL = "https://www.graphqlhub.com/playground"
            , apiURL = "https://metaphysics-production.artsy.net/"

            -- , apiURL = "https://graphql.anilist.co/"
            }
    in
    ( model, introspect model )


view : Model -> Browser.Document Msg
view model =
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
                [ div [ class "col-3" ] [ viewSchema (Types.queries model.types model.schema) ]
                , div [ class "col-8" ]
                    [ case ( model.typeField, model.schemaType ) of
                        ( Just typeField, Just schemaType ) ->
                            viewSchemaForm typeField schemaType

                        _ ->
                            text ""
                    ]
                ]
            ]
        ]


viewSchemaForm : Types.TypeField -> Types.SchemaType -> Html Msg
viewSchemaForm typeField schemaType =
    div []
        [ h3 [] [ text (Maybe.withDefault schemaType.name typeField.name) ]
        , p [] [ text (Maybe.withDefault "" typeField.description) ]
        , form []
            [ div [] (List.indexedMap formField (Maybe.withDefault [] schemaType.fields))
            , button [ class "btn btn-primary" ] [ text "Submit" ]
            ]
        ]


formField : Int -> Types.TypeField -> Html Msg
formField index field =
    let
        fieldName =
            "field-" ++ String.fromInt index
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text (Maybe.withDefault "fieldName" field.name) ]
        , input [ type_ "text", class "form-control" ] []
        , small [ class "form-text text-muted" ] [ text (Maybe.withDefault "" field.description) ]
        ]


viewAlert : Maybe Alert -> Html Msg
viewAlert alertMaybe =
    case alertMaybe of
        Just alert ->
            div [ class "row-fluid mt-5" ]
                [ div [ class ("alert alert-" ++ alert.category) ] [ text alert.message ] ]

        Nothing ->
            text ""


viewSchema : List ( Types.TypeField, Types.SchemaType ) -> Html Msg
viewSchema list =
    let
        _ =
            Debug.log "viewSchema" list
    in
    div [ class "list-group" ]
        (List.map viewSchemaType list)


viewSchemaType : ( Types.TypeField, Types.SchemaType ) -> Html Msg
viewSchemaType ( typeField, schemaType ) =
    a
        [ class "list-group-item list-group-item-action"
        , href "#"
        , onClick (ChosenSchema ( typeField, schemaType ))
        ]
        [ h5 [ class "mb-1" ] [ text (Maybe.withDefault schemaType.name typeField.name) ]
        , p [ class "mb-1" ] [ text (Maybe.withDefault "" schemaType.description) ]

        -- , small [] [ pre [] [ text (Debug.toString schemaType) ] ]
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

        OnHttpResponse (Ok schema) ->
            ( { model | alert = Nothing, schema = schema, types = Types.introspectedDict schema }, Cmd.none )

        OnHttpResponse (Err oops) ->
            ( { model | alert = Just { category = "danger", message = Debug.toString oops } }, Cmd.none )

        ChosenSchema ( typeField, schemaType ) ->
            ( { model | schemaType = Just schemaType, typeField = Just typeField }, Cmd.none )

        ApiUrlUpdated ->
            ( model, introspect model )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


introspect : Model -> Cmd Msg
introspect model =
    API.introspect model.apiURL
        |> Task.attempt OnHttpResponse
