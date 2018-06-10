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
                        , FadeAnimation.toHide
                        , FadeAnimation.toFadeIn (5 * second)
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
                        [ FadeAnimation.toShow
                        , FadeAnimation.wait (2 * second)
                        , FadeAnimation.toFadeOut (2 * second)
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
        [ renderContainer model
        , button
            [ onClick FadeIn
            ]
            [ text "Click to fade-in animate!" ]
        , button
            [ onClick FadeOut
            ]
            [ text "Click to fade-out animate!" ]
        ]


renderContainer : Model -> Html Msg
renderContainer model =
    let
        state =
            FadeAnimation.render
                model.state
    in
    div
        [ class <| className state ]
        [ text "This container is animating." ]


className : FadeAnimation.State -> String
className state =
    case state of
        FadeAnimation.FadeIn ->
            "container fadeIn"

        FadeAnimation.FadeOut ->
            "container fadeOut"

        FadeAnimation.Hide ->
            "container hide"

        FadeAnimation.Show ->
            "container"
