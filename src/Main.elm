module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import API
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import GraphQL
import Html exposing (Html, a, button, div, form, h1, h3, h5, input, label, main_, nav, node, p, pre, small, text, ul)
import Html.Attributes exposing (class, for, href, id, rel, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Json.Decode
import Task exposing (Task)
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
    | ChosenSchema GraphQL.Field
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

            -- , apiURL = "https://gitlab.com/api/graphql"
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
                [ div [ class "col-3" ]
                    [ model.schema
                        |> Maybe.map .queryType
                        |> Maybe.map (\t -> GraphQL.queries t model.types)
                        |> Maybe.map viewSchema
                        |> Maybe.withDefault (text "")
                    ]
                , div [ class "col" ]
                    [ div [ class "row" ]
                        (case ( model.field, model.type_ ) of
                            ( Just field, Just type_ ) ->
                                [ div [ class "col" ]
                                    [ viewSchemaForm field type_ ]
                                , div [ class "col-4" ] [ pre [] [ text "hello" ] ]
                                ]

                            _ ->
                                []
                        )
                    ]
                ]
            ]
        ]


viewSchemaForm : GraphQL.Field -> GraphQL.Type -> Html Msg
viewSchemaForm field type_ =
    div []
        [ h3 [] [ text field.name ]
        , p [] [ text (Maybe.withDefault "" field.description) ]
        , form []
            [-- div [] (List.indexedMap formField (Maybe.withDefault [] type_.fields))
             -- , button [ class "btn btn-primary" ] [ text "Submit" ]
            ]
        ]


formField : Int -> GraphQL.Field -> Html Msg
formField index field =
    let
        fieldName =
            "field-" ++ String.fromInt index
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text field.name ]
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


viewSchema : List GraphQL.Field -> Html Msg
viewSchema list =
    div [ class "list-group" ]
        (List.map viewSchemaType list)


viewSchemaType : GraphQL.Field -> Html Msg
viewSchemaType field =
    a
        [ class "list-group-item list-group-item-action"
        , href "#"
        , onClick (ChosenSchema field)
        ]
        [ h5 [ class "mb-1" ] [ text field.name ]
        , p [ class "mb-1" ] [ text (Maybe.withDefault "" field.description) ]

        -- , small [] [ pre [] [ text (Debug.toString type_) ] ]
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

        ChosenSchema field ->
            ( { model
                | type_ = Debug.log "type_" (GraphQL.fieldType model.types field)
                , field = Debug.log "field" (Just field)
              }
            , Cmd.none
            )

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
