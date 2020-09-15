port module TestWrapper exposing (main)

import Climate


main : Program () () Msg
main =
    Platform.worker
        { init = always ((), Cmd.none)
        , update = update
        , subscriptions = subscriptions
        }


port input : (Climate.Config -> msg) -> Sub msg
port output : Climate.Simulation -> Cmd msg


subscriptions : () -> Sub Msg
subscriptions _ =
    input Run

type Msg
    = Run Climate.Config

update : Msg -> () -> ((), Cmd Msg)
update msg _ =
    case msg of
        Run config ->
            ( ()
            , config
                |> Climate.simulate
                |> output
            )
