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
import Task
import Time exposing (Time)
import Tuple


type FadeAnimation
    = FadeAnimation
        { steps : List Step
        , interruptions : List Step
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
        , interruptions = []
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
            | interruptions = steps
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
        readyInterruptions =
            model.interruptions

        -- if there is more than one matching interruptions,
        -- we only take the first, which is the one that was most recently assigned.
        -- If an interruption does occur, we need to clear any interpolation overrides.
        steps =
            if List.length readyInterruptions > 0 then
                readyInterruptions
            else
                model.steps

        motion =
            model.motion

        ( revisedMotion, sentMessages, revisedSteps ) =
            resolveSteps motion steps

        _ =
            Debug.log "revisedSteps" revisedSteps

        _ =
            Debug.log "revisedMotion" revisedMotion
    in
    case tick of
        AnimationEnd ->
            ( FadeAnimation
                { model
                    | running =
                        List.length revisedSteps
                            /= 0
                    , steps = revisedSteps
                    , motion = revisedMotion
                }
            , Cmd.batch <| List.map (\m -> Task.perform identity (Task.succeed m)) sentMessages
            )

        Tick now ->
            if List.length readyInterruptions > 0 then
                ( FadeAnimation
                    { model
                        | running =
                            List.length revisedSteps
                                /= 0
                        , steps = revisedSteps
                        , interruptions = []
                        , motion = revisedMotion
                    }
                , Cmd.batch <| List.map (\m -> Task.perform identity (Task.succeed m)) sentMessages
                )
            else if (motion == Show) || (motion == Hide) then
                ( FadeAnimation
                    { model
                        | running =
                            List.length revisedSteps
                                /= 0
                        , steps = revisedSteps
                        , interruptions = []
                        , motion = revisedMotion
                    }
                , Cmd.batch <| List.map (\m -> Task.perform identity (Task.succeed m)) sentMessages
                )
            else
                ( FadeAnimation model, Cmd.none )


resolveSteps : Motion -> List Step -> ( Motion, List msg, List Step )
resolveSteps currentMotion steps =
    case List.head steps of
        Nothing ->
            case currentMotion of
                FadeIn ->
                    ( Show, [], [] )

                FadeOut ->
                    ( Hide, [], [] )

                _ ->
                    ( currentMotion, [], [] )

        Just currentStep ->
            case currentStep of
                To target ->
                    ( target
                    , []
                    , List.drop 1 steps
                    )


render : FadeAnimation -> Motion
render (FadeAnimation model) =
    model.motion
