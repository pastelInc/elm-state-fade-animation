module FadeAnimation exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Task
import Tuple


type FadeAnimation
    = FadeAnimation
        { steps : List Step
        , style : Property
        , running : Bool
        }


type Step
    = To Property


type Property
    = FadeIn
    | Show
    | FadeOut
    | Hide


type alias Msg =
    Tick


type Tick
    = Tick


type alias KeyFrame msg =
    { fadeIn : Html msg
    , show : Html msg
    , hide : Html msg
    , fadeOut : Html msg
    }


state : Property -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , style = current
        , running = False
        }


to : Property -> Step
to =
    To


queue : List Step -> FadeAnimation -> FadeAnimation
queue steps (FadeAnimation model) =
    update Tick <|
        FadeAnimation
            { model
                | steps = model.steps ++ steps
            }


onAnimationend : (Msg -> msgB) -> Html.Attribute msgB
onAnimationend msg =
    Html.Attributes.map msg <|
        Html.Events.on "animationend" (Json.Decode.succeed Tick)


isRunning : FadeAnimation -> Bool
isRunning (FadeAnimation model) =
    model.running


update : Msg -> FadeAnimation -> FadeAnimation
update tick animation =
    Tuple.first <| updateAnimation tick animation


updateAnimation : Tick -> FadeAnimation -> ( FadeAnimation, Cmd msg )
updateAnimation Tick (FadeAnimation model) =
    let
        -- if there is more than one matching interruptions,
        -- we only take the first, which is the one that was most recently assigned.
        -- If an interruption does occur, we need to clear any interpolation overrides.
        ( steps, style ) =
            ( model.steps, model.style )

        ( revisedStyle, sentMessages, revisedSteps ) =
            resolveSteps style steps

        _ =
            Debug.log "revisedSteps" revisedSteps

        _ =
            Debug.log "revisedStyle" revisedStyle
    in
    ( FadeAnimation
        { model
            | running =
                revisedStyle
                    /= Show
                    && revisedStyle
                    /= Hide
            , steps = revisedSteps
            , style = revisedStyle
        }
    , Cmd.batch <| List.map (\m -> Task.perform identity (Task.succeed m)) sentMessages
    )


resolveSteps : Property -> List Step -> ( Property, List msg, List Step )
resolveSteps style steps =
    case List.head steps of
        Nothing ->
            case style of
                FadeIn ->
                    ( Show, [], [] )

                FadeOut ->
                    ( Hide, [], [] )

                _ ->
                    ( style, [], [] )

        Just currentStep ->
            case currentStep of
                To target ->
                    ( target
                    , []
                    , List.drop 1 steps
                    )


render : KeyFrame msg -> FadeAnimation -> Html msg
render frame (FadeAnimation model) =
    case model.style of
        FadeIn ->
            frame.fadeIn

        Show ->
            frame.show

        Hide ->
            frame.hide

        FadeOut ->
            frame.fadeOut
