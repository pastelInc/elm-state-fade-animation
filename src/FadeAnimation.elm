module FadeAnimation
    exposing
        ( FadeAnimation
        , Motion(..)
        , Msg
        , interrupt
        , onAnimationend
        , queue
        , render
        , state
        , subscription
        , to
        , update
        )

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Time exposing (Time)
import Tuple


type FadeAnimation
    = FadeAnimation
        { steps : List Step
        , interruption : List Step
        , motion : Motion
        , running : Bool
        }


type Step
    = To Motion


type Motion
    = FadeIn
    | Show
    | FadeOut
    | Hide


type alias Msg =
    Tick


type Tick
    = Tick Time
    | AnimationEnd


state : Motion -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , interruption = []
        , motion = current
        , running = False
        }


to : Motion -> Step
to =
    To


queue : List Step -> FadeAnimation -> FadeAnimation
queue steps (FadeAnimation model) =
    FadeAnimation
        { model
            | steps = model.steps ++ steps
            , running = True
        }


interrupt : List Step -> FadeAnimation -> FadeAnimation
interrupt steps (FadeAnimation model) =
    FadeAnimation
        { model
            | interruption = steps
            , running = True
        }


onAnimationend : (Msg -> msgB) -> Html.Attribute msgB
onAnimationend msg =
    Html.Attributes.map msg <|
        Html.Events.on "animationend" (Json.Decode.succeed AnimationEnd)


{-| Create a subscription to AnimationFrame.times.

It is throttled based on whether the current animation is running or not.

-}
subscription : (Msg -> msgB) -> List FadeAnimation -> Sub msgB
subscription msg states =
    if List.any isRunning states then
        Sub.map msg (AnimationFrame.times Tick)
    else
        Sub.none


isRunning : FadeAnimation -> Bool
isRunning (FadeAnimation model) =
    model.running


update : Msg -> FadeAnimation -> FadeAnimation
update tick animation =
    Tuple.first <| updateAnimation tick animation


updateAnimation : Tick -> FadeAnimation -> ( FadeAnimation, Cmd msg )
updateAnimation tick (FadeAnimation model) =
    let
        ( readyInterruption, queuedInterruption ) =
            ( model.interruption, [] )

        isInterrupt =
            List.length readyInterruption > 0

        ( steps, motion ) =
            if isInterrupt then
                ( readyInterruption, model.motion )
            else
                ( model.steps, model.motion )

        ( revisedMotion, sentMessages, revisedSteps ) =
            resolveSteps motion steps tick isInterrupt

        _ =
            Debug.log "revisedSteps" revisedSteps

        _ =
            Debug.log "revisedMotion" revisedMotion
    in
    ( FadeAnimation
        { model
            | running =
                List.length revisedSteps
                    /= 0
            , steps = revisedSteps
            , motion = revisedMotion
            , interruption = queuedInterruption
        }
    , Cmd.none
    )


resolveSteps : Motion -> List Step -> Tick -> Bool -> ( Motion, List msg, List Step )
resolveSteps currentMotion steps tick forceResolve =
    case List.head steps of
        Nothing ->
            ( currentMotion, [], [] )

        Just currentStep ->
            case currentStep of
                To target ->
                    if alreadyAnimationEnd tick then
                        ( target
                        , []
                        , List.drop 1 steps
                        )
                    else if forceResolve then
                        ( target
                        , []
                        , List.drop 1 steps
                        )
                    else if (currentMotion == Show) || (currentMotion == Hide) then
                        ( target
                        , []
                        , List.drop 1 steps
                        )
                    else
                        ( currentMotion, [], steps )


alreadyAnimationEnd : Tick -> Bool
alreadyAnimationEnd tick =
    tick == AnimationEnd


render : FadeAnimation -> Motion
render (FadeAnimation model) =
    model.motion
