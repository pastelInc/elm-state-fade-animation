module FadeAnimation1
    exposing
        ( FadeAnimation
        , Motion(..)
        , Msg
        , interrupt
        , override
        , render
        , set
        , state
        , update
        )

import Process
import Task
import Time exposing (Time, millisecond)


-- 1フレーム単位の割り込みを行いたい
-- TODO: 同じアニメーションの割り込み処理を実現する


type FadeAnimation
    = FadeAnimation
        { steps : List Step
        , motion : Motion
        }


type Step
    = Override Motion -- 必ずアニメーションのはじめから再生する
    | Set Motion -- アニメーション呼び出し


type Motion
    = FadeIn
    | FadeOut
    | Show
    | Hide


type alias Msg =
    Tick


type Tick
    = Tick
    | AnimationEnd


state : Motion -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , motion = current
        }


override : Motion -> Step
override =
    Override


set : Motion -> Step
set =
    Set


interrupt : Step -> FadeAnimation -> ( FadeAnimation, Cmd Msg )
interrupt step (FadeAnimation model) =
    update Tick <|
        FadeAnimation
            { model
                | steps = [ step ]
            }


after : Float -> Time -> msg -> Cmd msg
after time unit msg =
    Process.sleep (time * unit) |> Task.perform (always msg)


update : Msg -> FadeAnimation -> ( FadeAnimation, Cmd Msg )
update tick (FadeAnimation ({ steps, motion } as model)) =
    let
        ( revisedMotion, revisedSteps ) =
            resolveSteps motion steps
    in
    ( FadeAnimation
        { model
            | steps = revisedSteps
            , motion = revisedMotion
        }
    , if isPause revisedMotion then
        -- NOTE: 16.666ms = 1 frame
        after 16.666 millisecond Tick
      else
        Cmd.none
    )


isPause : Motion -> Bool
isPause target =
    target == Show || target == Hide


resolveSteps : Motion -> List Step -> ( Motion, List Step )
resolveSteps currentMotion steps =
    case steps of
        [] ->
            ( currentMotion, [] )

        currentStep :: queuedSteps ->
            case currentStep of
                Set target ->
                    ( target
                    , queuedSteps
                    )

                Override target ->
                    ( startTowards target
                    , Set target :: queuedSteps
                    )


startTowards : Motion -> Motion
startTowards target =
    case target of
        FadeIn ->
            Hide

        FadeOut ->
            Show

        _ ->
            target


render : FadeAnimation -> Motion
render (FadeAnimation model) =
    model.motion
