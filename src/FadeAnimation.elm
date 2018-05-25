module FadeAnimation
    exposing
        ( FadeAnimation
        , Msg
        , State(..)
        , fadeIn
        , fadeOut
        , hide
        , interrupt
        , render
        , show
        , spring
        , state
        , subscription
        , update
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


type Spring
    = Spring
        { from : Step
        , to : Step
        }


type State
    = FadeIn
    | FadeOut
    | Hide
    | Show


type Msg
    = Tick Time


type alias Step =
    { state : State
    , dt : Time
    }


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


fadeIn : Time -> Step
fadeIn dt =
    { state = FadeIn
    , dt = dt
    }


fadeOut : Time -> Step
fadeOut dt =
    { state = FadeOut
    , dt = dt
    }


hide : Step
hide =
    { state = Hide
    , dt = 0
    }


show : Step
show =
    { state = Show
    , dt = 0
    }


spring : Step -> Step -> Spring
spring from to =
    Spring
        { from = from
        , to = to
        }


interrupt : Spring -> FadeAnimation -> FadeAnimation
interrupt (Spring { from, to }) (FadeAnimation model) =
    FadeAnimation
        { model
            | steps = [ from, to ]
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
resolveSteps currentState steps n =
    case steps of
        [] ->
            ( currentState, [] )

        { state, dt } :: queuedSteps ->
            if dt <= 0 then
                ( state
                , queuedSteps
                )
            else
                ( state
                , { state = state, dt = dt - n } :: queuedSteps
                )


render : FadeAnimation -> State
render (FadeAnimation model) =
    model.state
