module Main exposing (..)

import FadeAnimation
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
    Sub.batch []


type Msg
    = FadeIn
    | FadeOut
    | Animate FadeAnimation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FadeIn ->
            ( { model
                | state =
                    FadeAnimation.queue
                        [ FadeAnimation.to FadeAnimation.FadeIn
                        ]
                        model.state
              }
            , Cmd.none
            )

        FadeOut ->
            ( { model
                | state =
                    FadeAnimation.queue
                        [ FadeAnimation.to FadeAnimation.FadeOut
                        ]
                        model.state
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model
                | state = FadeAnimation.update animMsg model.state
              }
            , Cmd.none
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
            , button
                [ onClick <| Animate FadeAnimation.Tick
                ]
                [ text "Click to emit animationend!" ]
            ]


renderFadeContainer : Model -> Html Msg
renderFadeContainer model =
    FadeAnimation.render
        { fadeIn = renderFadeIn
        , show = renderShow
        , hide = renderHide
        , fadeOut = renderFadeOut
        }
        model.state


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
        , FadeAnimation.onAnimationend <| Animate FadeAnimation.Tick
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
        , FadeAnimation.onAnimationend <| Animate FadeAnimation.Tick
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
