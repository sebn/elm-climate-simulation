port module TestWrapper exposing (main)

import Climate exposing (SimulationValues)
import Json.Decode as JD
import Json.Encode as JE


main : Program () () Msg
main =
    Platform.worker
        { init = always ( (), Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port input : (JD.Value -> msg) -> Sub msg


port output : JE.Value -> Cmd msg


subscriptions : () -> Sub Msg
subscriptions _ =
    input Run


type Msg
    = Run JD.Value


update : Msg -> () -> ( (), Cmd Msg )
update msg _ =
    case msg of
        Run json ->
            ( ()
            , output <|
                case Climate.fromSimClimat json of
                    Ok sv ->
                        sv
                            |> Climate.simulate
                            |> Climate.toSimClimat

                    Err err ->
                        err
                            |> JD.errorToString
                            |> JE.string
            )
