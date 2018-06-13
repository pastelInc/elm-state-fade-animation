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
            FadeAnimation.state FadeAnimation.Show
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
                        [ FadeAnimation.wait (1 * second)
                        , FadeAnimation.playback 0 FadeAnimation.Hide
                        , FadeAnimation.playback (5 * second) FadeAnimation.FadeIn
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
                        [ FadeAnimation.playback 0 FadeAnimation.Show
                        , FadeAnimation.wait (2 * second)
                        , FadeAnimation.playback (2 * second) FadeAnimation.FadeOut
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
        [ viewContainer model
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


config : FadeAnimation.Config String
config =
    FadeAnimation.config
        { fadeIn = "container fadeIn"
        , fadeOut = "container fadeOut"
        , hide = "container hide"
        , show = "container"
        }
