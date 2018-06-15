module FadeAnimation
    exposing
        ( Config
        , FadeAnimation
        )

import AnimationFrame
import Time exposing (Time)


type FadeAnimation
    = FadeAnimation
        { appear : Bool
        , enter : Bool
        , into : Bool
        , status : Status
        , nextStatus : Maybe Status
        }


init : Bool -> FadeAnimation
init into =
    if into then
        FadeAnimation
            { appear = False
            , enter = False
            , into = into
            , status = Exited
            , nextStatus = Just Entering
            }
    else
        FadeAnimation
            { appear = False
            , enter = False
            , into = into
            , status = Exited
            , nextStatus = Nothing
            }


updateStatus : FadeAnimation -> FadeAnimation
updateStatus ((FadeAnimation { status, nextStatus }) as current) =
    case nextStatus of
        Just nextStatus_ ->
            case nextStatus_ of
                Entering ->
                    --performEnter
                    current

                _ ->
                    --performExit
                    current

        Nothing ->
            case status of
                Exited ->
                    --Unmounted
                    current

                _ ->
                    current


performEnter : FadeAnimation -> FadeAnimation
performEnter current =
    --if not enter then
    --    ( Entered, onEntered )
    --else
    --    let
    --        _ =
    --            onEnter
    --    in
    --    Entering
    current


type Status
    = Entering
    | Entered
    | Exiting
    | Exited


type Config
    = Config
        { into : Bool
        , classNames : String
        , timeout : Time
        , enter : Bool
        }


config :
    { into : Bool
    , classNames : String
    , timeout : Time
    , enter : Bool
    }
    -> Config
config { into, classNames, timeout, enter } =
    Config
        { into = into
        , classNames = classNames
        , timeout = timeout
        , enter = enter
        }


render : Config -> FadeAnimation -> String
render (Config { into, classNames, timeout, enter }) (FadeAnimation { status }) =
    ""
