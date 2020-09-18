module Climate exposing
    ( Config
    , Simulation
    , simulate
    )

import ExperienceValues as EV exposing (ExperienceValues)
import ModelPhysicsConstants
import PhysicsConstants


type alias Config =
    { annee_debut : Float
    , fixed_eau : Bool
    , fixed_concentration : Bool
    , debranche_biologie : Bool
    , fixed_ocean : Bool
    , debranche_ocean : Bool
    , fixed_albedo : Bool
    , rapport_H2O_value : Float
    , puit_bio_value : Float
    , puit_oce_value : Float
    , albedo_value : Float
    , coo_concentr_value : Float
    , puissance_soleil_value : Float
    , distance_ts_value : Float
    , obliquite_value : Float
    , excentricite_value : Float
    , precession_value : Float
    , alteration_value : Float
    , emit_anthro_coo_value : Float
    , volcan_value : Float
    , stockage_biologique_value : Float
    }


type alias Simulation =
    { -- CONFIG
      annee_debut : Float
    , fixed_eau : Bool
    , fixed_concentration : Bool
    , debranche_biologie : Bool
    , fixed_ocean : Bool
    , debranche_ocean : Bool
    , fixed_albedo : Bool
    , rapport_H2O_value : Float
    , puit_bio_value : Float
    , puit_oce_value : Float
    , albedo_value : Float
    , coo_concentr_value : Float
    , puissance_soleil_value : Float
    , distance_ts_value : Float
    , obliquite_value : Float
    , excentricite_value : Float
    , precession_value : Float
    , alteration_value : Float
    , emit_anthro_coo_value : Float
    , volcan_value : Float
    , stockage_biologique_value : Float

    -- RESULTS
    , temperature_data : DataArray Float
    }


type alias DataArray a =
    { -- N : Int
      datas : List a
    , past_datas : List a
    , resolution : Int
    , indice_min : Int
    , indice_max : Int
    , imin : Int
    , imax : Int
    }


n : Int
n =
    100


temperature_data_array : Config -> DataArray Float
temperature_data_array config =
    { datas = temperature_data config
    , past_datas = temperature_past_data config.annee_debut
    , resolution = 100
    , indice_min = 0
    , indice_max = 100
    , imin = 0
    , imax = 100
    }


temperature_data : Config -> List Float
temperature_data config =
    let
        ev =
            EV.fromEcheance 10000
    in
    zT_ancien config
        :: (List.range 1 n
                -- 14.399999999999977
                |> List.map (tau_niveau_calottes config)
           )


tau_niveau_calottes : Config -> Int -> Float
tau_niveau_calottes sv t =
    let
        -- FIXME
        zphig_ancien =
            50.0
    in
    if zphig_ancien < phieq sv t then
        PhysicsConstants.tau_niveau_calottes_deglacement

    else
        PhysicsConstants.tau_niveau_calottes_englacement


phieq : Config -> Int -> Float
phieq sv t =
    let
        -- FIXME
        zT =
            toFloat t + PhysicsConstants.tKelvin
    in
    (ModelPhysicsConstants.a_calottes
        * (zT - PhysicsConstants.tKelvin)
        + ModelPhysicsConstants.b_calottes
        + PhysicsConstants.c_calottes
        * (insol65N sv
            - ModelPhysicsConstants.insol_actuel
          )
    )
        |> min PhysicsConstants.niveau_calottes_max
        |> max PhysicsConstants.niveau_calottes_min


zT_ancien : Config -> Float
zT_ancien sv =
    (+) PhysicsConstants.tKelvin <|
        case truncate sv.annee_debut of
            1750 ->
                PhysicsConstants.temperature_1750

            _ ->
                PhysicsConstants.temperature_actuelle


alteration_max : Config -> Float
alteration_max sv =
    ModelPhysicsConstants.c_alteration_naturel * (sv.alteration_value / 100.0)


b_ocean : Config -> Float
b_ocean sv =
    if sv.debranche_ocean then
        0

    else if sv.fixed_ocean then
        0

    else
        PhysicsConstants.b_ocean


{-| Insolation 65º lat. N
-}
insol65N : Config -> Float
insol65N sv =
    fin0 sv
        * cos (delta_angle sv)
        * exp
            (2
                * log
                    ((1
                        - PhysicsConstants.excentricite_actuel
                        * 0.5
                        * sin (-PhysicsConstants.precession_actuel / 180 * pi)
                     )
                        / (1
                            - (0.3 * sv.excentricite_value + 0.7 * PhysicsConstants.excentricite_actuel)
                            * 0.5
                            * sin (-sv.precession_value / 180 * pi)
                          )
                    )
            )


delta_angle : Config -> Float
delta_angle sv =
    abs
        ((toFloat PhysicsConstants.lat_Mil - sv.obliquite_value)
            / 360
            * 2
            * PhysicsConstants.pi
        )


fin0 : Config -> Float
fin0 sv =
    -- Fin0 = CPhysicsConstants.puissance_recue_zero * (this.simulationValues.puissance_soleil_value / 100.) / (this.simulationValues.distance_ts_value / 100) / (this.simulationValues.distance_ts_value / 100);
    PhysicsConstants.puissance_recue_zero
        * (sv.puissance_soleil_value / 100.0)
        / (sv.distance_ts_value / 100)
        / (sv.distance_ts_value / 100)


dt : ExperienceValues -> Float
dt ev =
    EV.temps_elem ev / toFloat (niter ev)


niter : ExperienceValues -> Int
niter ev =
    max 4 (truncate (3 * exp (0.3 * log (EV.temps_elem ev))))


exp : Float -> Float
exp =
    (^) e


log : Float -> Float
log =
    logBase e



--  Math.max(4, Math.trunc(3 * Math.exp(0.3 * Math.log(this.experienceValues.temps_elem()))));


temperature_past_data : Float -> List Float
temperature_past_data annee_debut =
    (::) 0 <|
        case truncate annee_debut of
            1750 ->
                List.repeat n 14.399999999999977

            _ ->
                List.map temperature_past_value (List.range 1 100)


temperature_past_value : Int -> Float
temperature_past_value t =
    PhysicsConstants.temperature_actuelle
        - (tempsElem
            / internEcheance
            * toFloat t
            * PhysicsConstants.deltaT_last_century
          )
        -- FIXME: converting to then from kelvins introduces rounding errors
        + PhysicsConstants.tKelvin
        - PhysicsConstants.tKelvin


tempsElem : Float
tempsElem =
    1.0


internEcheance : Float
internEcheance =
    100.0


simulate : Config -> Simulation
simulate config =
    { annee_debut = config.annee_debut
    , fixed_eau = config.fixed_eau
    , fixed_concentration = config.fixed_concentration
    , debranche_biologie = config.debranche_biologie
    , fixed_ocean = config.fixed_ocean
    , debranche_ocean = config.debranche_ocean
    , fixed_albedo = config.fixed_albedo
    , rapport_H2O_value = config.rapport_H2O_value
    , puit_bio_value = config.puit_bio_value
    , puit_oce_value = config.puit_oce_value
    , albedo_value = config.albedo_value
    , coo_concentr_value = config.coo_concentr_value
    , puissance_soleil_value = config.puissance_soleil_value
    , distance_ts_value = config.distance_ts_value
    , obliquite_value = config.obliquite_value
    , excentricite_value = config.excentricite_value
    , precession_value = config.precession_value
    , alteration_value = config.alteration_value
    , emit_anthro_coo_value = config.emit_anthro_coo_value
    , volcan_value = config.volcan_value
    , stockage_biologique_value = config.stockage_biologique_value
    , temperature_data = temperature_data_array config
    }
