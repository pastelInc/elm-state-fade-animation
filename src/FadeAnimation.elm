module FadeAnimation
    exposing
        ( FadeAnimation
        , Motion(..)
        , Msg
        , onAnimationend
        , render
        , start
        , state
        , to
        , update
        )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Task
import Tuple


type FadeAnimation
    = FadeAnimation
        { steps : List Step
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
    = Tick


state : Motion -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , motion = current
        , running = False
        }


to : Motion -> Step
to =
    To


start : List Step -> FadeAnimation -> FadeAnimation
start steps (FadeAnimation model) =
    update Tick <|
        FadeAnimation
            { model
                | steps = model.steps ++ steps
            }


onAnimationend : (Msg -> msgB) -> Html.Attribute msgB
onAnimationend msg =
    Html.Attributes.map msg <|
        Html.Events.on "animationend" (Json.Decode.succeed Tick)


update : Msg -> FadeAnimation -> FadeAnimation
update tick animation =
    Tuple.first <| updateAnimation tick animation


updateAnimation : Tick -> FadeAnimation -> ( FadeAnimation, Cmd msg )
updateAnimation Tick (FadeAnimation model) =
    let
        -- if there is more than one matching interruptions,
        -- we only take the first, which is the one that was most recently assigned.
        -- If an interruption does occur, we need to clear any interpolation overrides.
        ( steps, motion ) =
            ( model.steps, model.motion )

        ( revisedMotion, sentMessages, revisedSteps ) =
            resolveSteps motion steps

        _ =
            Debug.log "revisedSteps" revisedSteps

        _ =
            Debug.log "revisedMotion" revisedMotion
    in
    ( FadeAnimation
        { model
            | running =
                revisedMotion
                    /= Show
                    && revisedMotion
                    /= Hide
            , steps = revisedSteps
            , motion = revisedMotion
        }
    , Cmd.batch <| List.map (\m -> Task.perform identity (Task.succeed m)) sentMessages
    )


resolveSteps : Motion -> List Step -> ( Motion, List msg, List Step )
resolveSteps motion steps =
    case List.head steps of
        Nothing ->
            case motion of
                FadeIn ->
                    ( Show, [], [] )

                FadeOut ->
                    ( Hide, [], [] )

                _ ->
                    ( motion, [], [] )

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
