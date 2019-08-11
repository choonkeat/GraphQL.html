module UI exposing
    ( Alert
    , alert
    , breadcrumbs
    , codeBlock
    , description
    , inputCheckbox
    , inputChoices
    , inputString
    , inputText
    , submitButton
    )

import Html exposing (Html, a, button, code, div, em, form, h1, h3, h5, hr, img, input, label, li, main_, nav, node, ol, option, p, pre, select, small, span, strong, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, for, href, id, name, placeholder, rel, required, src, style, target, title, type_, value)
import Html.Events exposing (on, onBlur, onClick, onInput, onSubmit)
import Json.Decode


type alias Alert =
    { category : String
    , message : String
    }


alert : Maybe Alert -> Html a
alert alertMaybe =
    case alertMaybe of
        Just a ->
            div [ class "row-fluid mt-5" ]
                [ div [ class ("alert alert-" ++ a.category) ] [ text a.message ] ]

        Nothing ->
            text ""


description : Maybe String -> Html a
description maybeString =
    small [ class "text-muted" ] [ text (Maybe.withDefault "" maybeString) ]


inputString : { label : List (Html a), htmlType : String, value : String, description : Html a, attrs : List (Html.Attribute a) } -> Html a
inputString config =
    div [ class "form-group" ]
        [ label [] config.label
        , input (List.append [ type_ config.htmlType, value config.value, class "form-control" ] config.attrs) []
        , config.description
        ]


inputText : { label : List (Html a), value : String, description : Html a, attrs : List (Html.Attribute a) } -> Html a
inputText config =
    div [ class "form-group" ]
        [ label [] config.label
        , textarea (List.append [ class "form-control" ] config.attrs) [ text config.value ]
        , config.description
        ]


inputCheckbox : { label : List (Html a), description : Html a, attrs : List (Html.Attribute a) } -> Html a
inputCheckbox config =
    div [ class "form-check" ]
        [ label [ class "form-check-label" ]
            (List.append [ input (List.append [ class "form-check-input", type_ "checkbox" ] config.attrs) [] ] config.label)
        ]


inputChoices : (String -> a) -> { required : Bool, label : List (Html a), description : Html a, attrs : List (Html.Attribute a), options : List String } -> Html a
inputChoices msgFn config =
    div [ class "form-group" ]
        [ label [] config.label
        , select
            (List.append [ on "change" (Json.Decode.map msgFn Html.Events.targetValue) ] config.attrs)
            (List.append
                (if config.required then
                    []

                 else
                    [ option [ value "" ] [ text "" ] ]
                )
                (List.map (\s -> option [ value s ] [ text s ]) config.options)
            )
        , config.description
        ]


submitButton : { loading : Bool } -> Html a
submitButton config =
    if config.loading then
        button [ type_ "submit", disabled True, class "btn btn-primary progress-bar-striped progress-bar-animated" ] [ text "Submit" ]

    else
        button [ type_ "submit", class "btn btn-primary" ] [ text "Submit" ]


codeBlock : { attrs : List (Html.Attribute a) } -> String -> Html a
codeBlock config string =
    pre (List.append [ style "white-space" "pre-wrap", style "word-break" "break-all", style "background-color" "lightgray" ] config.attrs)
        [ text string ]


breadcrumbs : List (Html a) -> List String -> String -> Html a
breadcrumbs htmlList crumbs lastCrumb =
    case crumbs of
        [] ->
            nav [ attribute "aria-label" "breadcrumb" ]
                [ ol [ class "breadcrumb" ]
                    (List.append
                        (List.reverse htmlList)
                        [ li [ class "breadcrumb-item", style "word-break" "break-all" ] [ text lastCrumb ] ]
                    )
                ]

        "" :: xs ->
            breadcrumbs htmlList xs lastCrumb

        x :: xs ->
            let
                currentHtml =
                    li [ class "breadcrumb-item" ]
                        [ a [ href (String.join "/" (List.repeat (List.length crumbs) "..")) ] [ text x ] ]
            in
            breadcrumbs (currentHtml :: htmlList) xs lastCrumb
