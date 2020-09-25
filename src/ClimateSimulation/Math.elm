module ClimateSimulation.Math exposing
    ( exp
    , log
    )


exp : Float -> Float
exp =
    (^) e


log : Float -> Float
log =
    logBase e
