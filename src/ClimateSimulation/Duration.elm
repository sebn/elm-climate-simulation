module ClimateSimulation.Duration exposing
    ( Duration
    , dt
    , fromYears
    , indice_max
    , indice_min
    , intoYears
    , niter
    , resolution
    , temps_elem
    )

import ClimateSimulation.Math exposing (exp, log)


type Duration
    = Duration Int


fromYears : Int -> Duration
fromYears echeance_ =
    Duration echeance_


intoYears : Duration -> Int
intoYears (Duration echeance_) =
    echeance_


indice_max : Duration -> Int
indice_max (Duration echeance_) =
    if echeance_ <= 100 then
        echeance_

    else
        100


indice_min : Duration -> Int
indice_min (Duration _) =
    -- if echeance_ <= 100 then
    --     -99
    -- else
    --     -100 // truncate (temps_elem (ExperienceValues echeance_))
    0


resolution : Duration -> Int
resolution (Duration years) =
    years // 100


temps_elem : Duration -> Float
temps_elem (Duration echeance_) =
    if echeance_ <= 100 then
        1.0

    else
        toFloat echeance_ / 100.0


dt : Duration -> Float
dt ev =
    temps_elem ev / toFloat (niter ev)


niter : Duration -> Int
niter ev =
    max 4 (truncate (3 * exp (0.3 * log (temps_elem ev))))
