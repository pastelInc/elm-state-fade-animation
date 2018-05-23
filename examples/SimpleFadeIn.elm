module Main exposing (..)

import FadeAnimation1 as FadeAnimation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { state : FadeAnimation.FadeAnimation
    }


init : ( Model, Cmd Msg )
init =
    ( { state =
            FadeAnimation.state FadeAnimation.Show
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = FadeIn
    | FadeOut
    | Animate FadeAnimation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FadeIn ->
            let
                ( newState, cmds ) =
                    FadeAnimation.interrupt
                        (FadeAnimation.override FadeAnimation.FadeIn)
                        model.state
            in
            ( { model
                | state = newState
              }
            , Cmd.map Animate cmds
            )

        FadeOut ->
            let
                ( newState, cmds ) =
                    FadeAnimation.interrupt
                        (FadeAnimation.override FadeAnimation.FadeOut)
                        model.state
            in
            ( { model
                | state = newState
              }
            , Cmd.map Animate cmds
            )

        Animate animMsg ->
            let
                ( newState, cmds ) =
                    FadeAnimation.update animMsg model.state
            in
            ( { model
                | state = newState
              }
            , Cmd.map Animate cmds
            )


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "render" model
    in
    div
        []
        [ renderFadeContainer model
        , button
            [ onClick FadeIn
            ]
            [ text "Click to fade-in animate!" ]
        , button
            [ onClick FadeOut
            ]
            [ text "Click to fade-out animate!" ]
        ]


renderFadeContainer : Model -> Html Msg
renderFadeContainer model =
    let
        keyFrame =
            FadeAnimation.render
                model.state
    in
    case keyFrame of
        FadeAnimation.FadeIn ->
            renderFadeIn

        FadeAnimation.Show ->
            renderShow

        FadeAnimation.Hide ->
            renderHide

        FadeAnimation.FadeOut ->
            renderFadeOut


renderFadeIn : Html Msg
renderFadeIn =
    div
        [ style
            [ ( "position", "relative" )
            , ( "margin", "100px auto" )
            , ( "padding", "25px" )
            , ( "width", "200px" )
            , ( "height", "200px" )
            , ( "background-color", "#268bd2" )
            , ( "color", "white" )
            , ( "opacity", "1" )
            ]
        , class "container fadeIn"
        ]
        [ text "Fade animation container" ]


renderShow : Html Msg
renderShow =
    div
        [ style
            [ ( "position", "relative" )
            , ( "margin", "100px auto" )
            , ( "padding", "25px" )
            , ( "width", "200px" )
            , ( "height", "200px" )
            , ( "background-color", "#268bd2" )
            , ( "color", "white" )
            , ( "opacity", "1" )
            ]
        , class "container"
        ]
        [ text "Fade animation container" ]


renderFadeOut : Html Msg
renderFadeOut =
    div
        [ style
            [ ( "position", "relative" )
            , ( "margin", "100px auto" )
            , ( "padding", "25px" )
            , ( "width", "200px" )
            , ( "height", "200px" )
            , ( "background-color", "#268bd2" )
            , ( "color", "white" )
            , ( "opacity", "0" )
            ]
        , class "container fadeOut"
        ]
        [ text "Fade animation container" ]


renderHide : Html Msg
renderHide =
    div
        [ style
            [ ( "position", "relative" )
            , ( "margin", "100px auto" )
            , ( "padding", "25px" )
            , ( "width", "200px" )
            , ( "height", "200px" )
            , ( "background-color", "#268bd2" )
            , ( "color", "white" )
            , ( "opacity", "0" )
            ]
        , class "container"
        ]
        [ text "Fade animation container" ]
