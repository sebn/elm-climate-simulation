module ClimateSimulation.Duration exposing
    ( Duration
    , fromYears
    , indice_max
    , indice_min
    , intoYears
    , temps_elem
    )


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


temps_elem : Duration -> Float
temps_elem (Duration echeance_) =
    if echeance_ <= 100 then
        1.0

    else
        toFloat echeance_ / 100.0
