module ClimateSimulation.ExperienceValues exposing
    ( ExperienceValues
    , echeance
    , fromEcheance
    , indice_max
    , indice_min
    , temps_elem
    )


type ExperienceValues
    = ExperienceValues Int


fromEcheance : Int -> ExperienceValues
fromEcheance echeance_ =
    ExperienceValues echeance_


echeance : ExperienceValues -> Int
echeance (ExperienceValues echeance_) =
    echeance_


indice_max : ExperienceValues -> Int
indice_max (ExperienceValues echeance_) =
    if echeance_ <= 100 then
        echeance_

    else
        100


indice_min : ExperienceValues -> Int
indice_min (ExperienceValues _) =
    -- if echeance_ <= 100 then
    --     -99
    -- else
    --     -100 // truncate (temps_elem (ExperienceValues echeance_))
    0


temps_elem : ExperienceValues -> Float
temps_elem (ExperienceValues echeance_) =
    if echeance_ <= 100 then
        1.0

    else
        toFloat echeance_ / 100.0
