module Main exposing (..)

import FadeAnimation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)


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
            FadeAnimation.visible
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    FadeAnimation.subscription Animate [ model.state ]


type Msg
    = FadeIn
    | FadeOut
    | Animate FadeAnimation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FadeIn ->
            let
                newState =
                    FadeAnimation.interrupt
                        [ FadeAnimation.hide
                        , FadeAnimation.fadeIn (5 * second)
                        ]
                        model.state
            in
            ( { model
                | state = newState
              }
            , Cmd.none
            )

        FadeOut ->
            let
                newState =
                    FadeAnimation.interrupt
                        [ FadeAnimation.show
                        , FadeAnimation.wait (2 * second)
                        , FadeAnimation.fadeOut (2 * second)
                        ]
                        model.state
            in
            ( { model
                | state = newState
              }
            , Cmd.none
            )

        Animate animMsg ->
            let
                newState =
                    FadeAnimation.update animMsg model.state
            in
            ( { model
                | state = newState
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "FadeAnimation" model
    in
    div
        []
        [ div [ class "containerWrapper" ]
            [ viewContainer model
            ]
        , button
            [ onClick FadeIn
            ]
            [ text "Click to fade-in animate!" ]
        , button
            [ onClick FadeOut
            ]
            [ text "Click to fade-out animate!" ]
        ]


viewContainer : Model -> Html Msg
viewContainer model =
    let
        classes =
            class <|
                FadeAnimation.render config model.state
    in
    div
        [ classes ]
        [ text "This container is animating." ]


config : FadeAnimation.Config
config =
    FadeAnimation.config
        { into = True
        , classNames = "container"
        , timeout = 5 * Time.second
        , enter = True

        --, onEnter = ()
        --, onEntering = ()
        --, onEntered = ()
        }
