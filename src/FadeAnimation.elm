module FadeAnimation
    exposing
        ( Config
        , FadeAnimation
        , Msg
        , State(FadeIn, FadeOut, Hide, Show)
        , config
        , interrupt
        , playback
        , render
        , state
        , subscription
        , update
        , wait
        )

import AnimationFrame
import Time exposing (Time)


type FadeAnimation
    = FadeAnimation
        { playlists : List Animation
        , state : State
        , timing : Timing
        , running : Bool
        }


type alias Timing =
    { current : Time
    , duration : Time
    }


type State
    = FadeIn
    | FadeOut
    | Hide
    | Show


type Msg
    = Tick Time


type Animation
    = Wait Time
    | Animation Time State


state : State -> FadeAnimation
state current =
    FadeAnimation
        { playlists = []
        , state = current
        , timing =
            { current = 0
            , duration = 0
            }
        , running = False
        }


playback : Time -> State -> Animation
playback =
    Animation


wait : Time -> Animation
wait =
    Wait


interrupt : List Animation -> FadeAnimation -> FadeAnimation
interrupt playlists (FadeAnimation model) =
    FadeAnimation
        { model
            | playlists = playlists
            , running = True
        }


subscription : (Msg -> msg) -> List FadeAnimation -> Sub msg
subscription msg states =
    if List.any isRunning states then
        Sub.map msg (AnimationFrame.times Tick)
    else
        Sub.none


isRunning : FadeAnimation -> Bool
isRunning (FadeAnimation model) =
    model.running


update : Msg -> FadeAnimation -> FadeAnimation
update (Tick now) (FadeAnimation ({ playlists, state } as model)) =
    let
        timing =
            refreshTiming now model.timing

        ( revisedState, revisedPlaylists ) =
            resolvePlaylists state playlists timing.duration
    in
    FadeAnimation
        { model
            | playlists = revisedPlaylists
            , state = revisedState
            , timing = timing
            , running =
                List.length revisedPlaylists /= 0
        }


refreshTiming : Time -> Timing -> Timing
refreshTiming now timing =
    let
        duration =
            now - timing.current

        -- NOTE:
        -- If duration is longer than 2 frames, it is fixed to the time of 1 frame.
        -- When duration is longer than 2 frames, it is as follows.
        --    + the user leaves the browser window and returns
        --    + subscription stops updating and starts updating again
        --
        -- If timing.current == 0, there are special cases that occur at startup.
    in
    { current = now
    , duration =
        if duration > 34 || timing.current == 0 then
            16.666
        else
            duration
    }


resolvePlaylists : State -> List Animation -> Time -> ( State, List Animation )
resolvePlaylists currentState playlists duration =
    case playlists of
        [] ->
            ( currentState, [] )

        animation :: queuedPlaylists ->
            case animation of
                Wait n ->
                    if n <= 0 then
                        resolvePlaylists currentState queuedPlaylists duration
                    else
                        ( currentState
                        , (Wait <| n - duration) :: queuedPlaylists
                        )

                Animation n target ->
                    ( target, List.reverse <| Wait n :: List.reverse queuedPlaylists )


type Config a
    = Config
        { fadeIn : a
        , fadeOut : a
        , hide : a
        , show : a
        }


config :
    { fadeIn : a
    , fadeOut : a
    , hide : a
    , show : a
    }
    -> Config a
config { fadeIn, fadeOut, hide, show } =
    Config
        { fadeIn = fadeIn
        , fadeOut = fadeOut
        , hide = hide
        , show = show
        }


render : Config a -> FadeAnimation -> a
render (Config { fadeIn, fadeOut, hide, show }) (FadeAnimation { state }) =
    case state of
        FadeIn ->
            fadeIn

        FadeOut ->
            fadeOut

        Hide ->
            hide

        Show ->
            show
