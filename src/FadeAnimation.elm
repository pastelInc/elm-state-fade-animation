module FadeAnimation
    exposing
        ( Config
        , FadeAnimation
        , Msg
        , config
        , init
        , render
        , subscription
        , update
        )

import AnimationFrame
import Time exposing (Time)


type FadeAnimation
    = FadeAnimation
        { in_ : Bool
        , status : Status
        , nextStatus : Maybe Status
        , mounting : Bool
        }


type Msg
    = Tick Time


init : Bool -> FadeAnimation
init in_ =
    if in_ then
        FadeAnimation
            { in_ = in_
            , status = Exited
            , nextStatus = Just Entering
            , mounting = False
            }
    else
        FadeAnimation
            { in_ = in_
            , status = Exited
            , nextStatus = Nothing
            , mounting = False
            }


{-| 初期レンダリング後のイベント発火
It like componentDidMount
-}
subscription : (Msg -> msgB) -> FadeAnimation -> Sub msgB
subscription msg (FadeAnimation { mounting }) =
    if not <| mounting then
        Sub.map msg (AnimationFrame.times Tick)
    else
        Sub.none


update : Msg -> FadeAnimation -> FadeAnimation
update msg (FadeAnimation model) =
    case msg of
        Tick _ ->
            FadeAnimation
                { model | mounting = True }


updateStatus : FadeAnimation -> FadeAnimation
updateStatus ((FadeAnimation { status, nextStatus }) as current) =
    case nextStatus of
        Just nextStatus_ ->
            case nextStatus_ of
                Entering ->
                    --performEnter
                    current

                Exiting ->
                    --performExit
                    current

                _ ->
                    let
                        _ =
                            Debug.log
                                "FadeAnimation"
                                "nextStatus will always be ENTERING or EXITING."
                    in
                    current

        Nothing ->
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
    | Unmounted


type Config
    = Config
        { in_ : Bool
        , classNames : String
        , timeout : Time
        }


config :
    { in_ : Bool
    , classNames : String
    , timeout : Time
    }
    -> Config
config { in_, classNames, timeout } =
    Config
        { in_ = in_
        , classNames = classNames
        , timeout = timeout
        }


render : Config -> FadeAnimation -> String
render (Config { classNames, timeout }) (FadeAnimation { status, nextStatus }) =
    case ( status, nextStatus ) of
        ( Exited, Just Entering ) ->
            classNames ++ "-enter"

        ( Entering, _ ) ->
            classNames ++ "-enter-active"

        ( Entered, Just Exiting ) ->
            classNames ++ "-exit"

        ( Exiting, _ ) ->
            classNames ++ "-exit-active"

        _ ->
            classNames
