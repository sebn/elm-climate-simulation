port module TestWrapper exposing (main)

import Climate exposing (SimulationValues)


main : Program () () Msg
main =
    Platform.worker
        { init = always ( (), Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port input : (SimulationValues -> msg) -> Sub msg


port output : SimulationValues -> Cmd msg


subscriptions : () -> Sub Msg
subscriptions _ =
    input Run


type Msg
    = Run SimulationValues


update : Msg -> () -> ( (), Cmd Msg )
update msg _ =
    case msg of
        Run config ->
            ( ()
            , config
                |> Climate.simulate
                |> output
            )
