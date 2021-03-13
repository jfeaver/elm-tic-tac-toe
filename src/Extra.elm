module Extra exposing (..)

import Process
import Task


delay : Float -> msg -> Cmd msg
delay seconds msg =
    Process.sleep (seconds * 1000)
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


isJust : Maybe a -> Bool
isJust maybe =
    maybe
        |> Maybe.map (always True)
        |> Maybe.withDefault False
