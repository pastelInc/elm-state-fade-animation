module FadeAnimation
    exposing
        ( FadeAnimation
        , Msg
        , State(..)
        , interrupt
        , render
        , state
        , subscription
        , toFadeIn
        , toFadeOut
        , toHide
        , toShow
        , update
        , wait
        )

import AnimationFrame
import Time exposing (Time)


type FadeAnimation
    = FadeAnimation
        { steps : List Step
        , state : State
        , timing : Timing
        , running : Bool
        }


type alias Timing =
    { current : Time
    , dt : Time
    }


type State
    = FadeIn
    | FadeOut
    | Hide
    | Show


type Msg
    = Tick Time


type Step
    = Wait Time
    | To State Time


state : State -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , state = current
        , timing =
            { current = 0
            , dt = 0
            }
        , running = False
        }


toFadeIn : Time -> Step
toFadeIn dt =
    To FadeIn dt


toFadeOut : Time -> Step
toFadeOut dt =
    To FadeOut dt


toHide : Step
toHide =
    To Hide 0


toShow : Step
toShow =
    To Show 0


wait : Time -> Step
wait =
    Wait


interrupt : List Step -> FadeAnimation -> FadeAnimation
interrupt steps (FadeAnimation model) =
    FadeAnimation
        { model
            | steps = steps
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
update (Tick now) (FadeAnimation ({ steps, state } as model)) =
    let
        -- set current and dt time
        timing =
            refreshTiming now model.timing

        ( revisedState, revisedSteps ) =
            resolveSteps state steps timing.dt
    in
    FadeAnimation
        { model
            | steps = revisedSteps
            , state = revisedState
            , timing = timing
            , running =
                List.length revisedSteps /= 0
        }


refreshTiming : Time -> Timing -> Timing
refreshTiming now timing =
    let
        dt =
            now - timing.current

        -- dt is set to one frame (16.66) if it is a large dt(more than 2 frames),
        -- A large dt means one of the following:
        --    * the user left the browser window and came back
        --    * the animation subscription has stopped calling for updates for a while and started running again
        --
        -- We also have a special case when timing.current == 0, which is happens at startup.
    in
    { current = now
    , dt =
        if dt > 34 || timing.current == 0 then
            16.666
        else
            dt
    }


resolveSteps : State -> List Step -> Time -> ( State, List Step )
resolveSteps currentState steps dt =
    case steps of
        [] ->
            ( currentState, [] )

        step :: queuedSteps ->
            case step of
                Wait n ->
                    if n <= 0 then
                        resolveSteps currentState queuedSteps dt
                    else
                        ( currentState
                        , (Wait <| n - dt) :: queuedSteps
                        )

                To target n ->
                    ( target, List.reverse <| Wait n :: List.reverse queuedSteps )


render : FadeAnimation -> State
render (FadeAnimation model) =
    model.state
