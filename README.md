# elm-fade-animation-assistant

This library help you to switch state for css animation.
It can construct sequence animation by combining multiple state.

## Usage

The initial state can be selected from `visible` or `hidden`.

```elm
import FadeAnimation


init : Model
init =
    { state =
        FadeAnimation.visible
    }
```

The `subscribe` updates state using AnimationFrame when sequence animation is running.

```elm
import FadeAnimation


subscriptions : Model -> Sub Msg
subscriptions model =
    FadeAnimation.subscription Animate [ model.state ]
```

Please set `Msg` to update state.

```elm
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
```

Please config what you want to render for each state.
Someting in accord with a state will return.

```elm
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
```
