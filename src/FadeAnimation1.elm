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
        , subscription
        , update
        )

import AnimationFrame
import Time exposing (Time)


-- 1フレーム単位の割り込みを行いたい
-- TODO: 同じアニメーションの割り込み処理を実現する


type FadeAnimation
    = FadeAnimation
        { steps : List Step
        , motion : Motion
        , running : Bool
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
    | OneFrame Time


state : Motion -> FadeAnimation
state current =
    FadeAnimation
        { steps = []
        , motion = current
        , running = False
        }


override : Motion -> Step
override =
    Override


set : Motion -> Step
set =
    Set


interrupt : Step -> FadeAnimation -> FadeAnimation
interrupt step (FadeAnimation model) =
    FadeAnimation
        { model
            | steps = [ step ]
            , running = True
        }


{-| Create a subscription to AnimationFrame.times.

It is throttled based on whether the current animation is running or not.
キューの中身がなくなるまでsubscribeし続けるが、要調整。

-}
subscription : (Msg -> msgB) -> List FadeAnimation -> Sub msgB
subscription msg states =
    if List.any isRunning states then
        Sub.map msg (AnimationFrame.times OneFrame)
    else
        Sub.none



-- Queueの残りがあればkickする


isRunning : FadeAnimation -> Bool
isRunning (FadeAnimation model) =
    model.running


update : Msg -> FadeAnimation -> FadeAnimation
update tick (FadeAnimation ({ steps, motion } as model)) =
    let
        ( revisedMotion, revisedSteps ) =
            resolveSteps motion steps
    in
    FadeAnimation
        { model
            | steps = revisedSteps
            , motion = revisedMotion
            , running =
                List.length steps > 0
        }


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
