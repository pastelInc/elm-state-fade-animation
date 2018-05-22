module FadeAnimation
    exposing
        ( FadeAnimation
        , Motion(..)
        , Msg
        , fadeIn
        , fadeOut
        , interrupt
        , onAnimationend
        , queue
        , render
        , state
        , subscription
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
        { steps : List Motion
        , interruption : List Motion
        , motion : Motion
        , running : Bool
        }


type Motion
    = FadeIn
    | Show
    | FadeOut
    | Hide


type alias Composition =
    List Motion


fadeIn : Composition
fadeIn =
    [ Hide
    , FadeIn
    , Show
    ]


fadeOut : Composition
fadeOut =
    [ Show
    , FadeOut
    , Hide
    ]


type alias Msg =
    Tick


type Tick
    = Tick Time
    | AnimationEnd


unzipComposition : List Composition -> List Motion
unzipComposition compositions =
    List.foldl (\composition steps -> List.concat [ composition, steps ]) [] compositions


state : Motion -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , interruption = []
        , motion = current
        , running = False
        }


queue : List Composition -> FadeAnimation -> FadeAnimation
queue compositions (FadeAnimation model) =
    let
        steps =
            unzipComposition compositions
    in
    FadeAnimation
        { model
            | steps = model.steps ++ steps
            , running = True
        }


interrupt : List Composition -> FadeAnimation -> FadeAnimation
interrupt compositions (FadeAnimation model) =
    let
        steps =
            unzipComposition compositions
    in
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


resolveSteps : Motion -> List Motion -> Tick -> Bool -> ( Motion, List msg, List Motion )
resolveSteps currentMotion steps tick forceResolve =
    case List.head steps of
        Nothing ->
            ( currentMotion, [], [] )

        Just target ->
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
