module Climate exposing
    ( Config
    , Simulation
    , simulate
    )

import ExperienceValues as EV exposing (ExperienceValues)
import List.Nonempty as NEList
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
    , temperature_data : DataArray Ancien
    }


type alias DataArray a =
    { -- N : Int
      datas : List a
    , past_datas : List Float
    , resolution : Int
    , indice_min : Int
    , indice_max : Int
    , imin : Int
    , imax : Int
    }


n : Int
n =
    100


temperature_data_array : Config -> DataArray Ancien
temperature_data_array sv =
    { datas = boucleT sv
    , past_datas = temperature_past_data sv.annee_debut
    , resolution = 100
    , indice_min = 0
    , indice_max = 100
    , imin = 0
    , imax = 100
    }


type alias Ancien =
    { alteration_max : Float
    , b_ocean : Float
    , fin : Float
    , insol65N : Float
    , phieq : Float
    , tau_niveau_calottes : Float
    , zB_ocean : Float
    , zC_alteration : Float
    , zT : Float
    , zT_ancien : Float
    , zalbedo : Float
    , zphig : Float
    , zphig_ancien : Float
    , zpuit_bio : Float
    , zpuit_oce : Float
    }


boucleT : Config -> List Ancien
boucleT sv =
    let
        zT0 =
            calcul_zT0 sv

        zphig0 =
            calcul_zphig0 sv

        ancien =
            { alteration_max = calcul_alteration_max sv
            , b_ocean = calcul_b_ocean sv
            , fin = calcul_fin0 sv
            , insol65N = insol65N sv
            , phieq = calcul_phieq sv zT0
            , tau_niveau_calottes = calcul_tau_niveau_calottes sv 0
            , zB_ocean = 0
            , zC_alteration = 0
            , zT = zT0
            , zT_ancien = zT0
            , zalbedo = 0
            , zphig = zphig0
            , zphig_ancien = zphig0
            , zpuit_bio = zpuit_bio sv
            , zpuit_oce = 0
            }
    in
    List.range 1 n
        |> List.foldl
            (calculsBoucleT sv)
            (NEList.fromElement ancien)
        |> NEList.reverse
        |> NEList.tail
        |> (::) ancien


calculsBoucleT : Config -> (Int -> NEList.Nonempty Ancien -> NEList.Nonempty Ancien)
calculsBoucleT sv t anciens =
    let
        ancien =
            NEList.head anciens
    in
    NEList.cons
        (boucleIter sv t ancien)
        anciens


ev =
    EV.fromEcheance 10000


calcul_zT0 sv =
    (+) PhysicsConstants.tKelvin <|
        case truncate sv.annee_debut of
            1750 ->
                PhysicsConstants.temperature_1750

            _ ->
                PhysicsConstants.temperature_actuelle


calcul_zphig0 sv =
    case truncate sv.annee_debut of
        1750 ->
            PhysicsConstants.niveau_calottes_1750

        _ ->
            ModelPhysicsConstants.niveau_calottes_actuel


zpuit_oce0 sv =
    if sv.fixed_concentration then
        -- undefined in SimClimat
        0

    else if sv.debranche_ocean then
        0

    else
        sv.puit_oce_value / 100


boucleIter : Config -> Int -> Ancien -> Ancien
boucleIter sv t ancien =
    List.range 1 niter
        |> List.foldl
            (calculsBoucleIter sv t)
            ancien


calculsBoucleIter : Config -> Int -> (Int -> Ancien -> Ancien)
calculsBoucleIter sv t iter ancien =
    let
        zT =
            ancien.zT

        tau_niveau_calottes =
            calcul_tau_niveau_calottes sv t

        zphig =
            calcul_zphig sv ancien tau_niveau_calottes

        zalbedo =
            calcul_albedo sv zphig
    in
    { ancien
        | fin = calcul_fin sv zalbedo
        , phieq = calcul_phieq sv zT
        , tau_niveau_calottes = tau_niveau_calottes
        , zB_ocean = calcul_zB_ocean ancien
        , zC_alteration = calcul_zC_alteration ancien.alteration_max ancien.zphig
        , zT = zT
        , zT_ancien = ancien.zT
        , zalbedo = zalbedo
        , zphig = zphig
        , zphig_ancien = ancien.zphig
        , zpuit_oce = calcul_zpuit_oce sv zT
    }


calcul_fin : Config -> Float -> Float
calcul_fin sv zalbedo =
    calcul_fin0 sv * (1 - zalbedo)


calcul_albedo : Config -> Float -> Float
calcul_albedo sv zphig =
    -- if (this.simulationValues.fixed_albedo) {
    --     zalbedo = this.simulationValues.albedo_value / 100.; // conversion albedo en % en albedo en unité
    --     for (var t = 0; t <= this.experienceValues.indice_max(); t++) {
    --         this.simulationValues.albedo_data.set(t, this.simulationValues.albedo_value);
    --     }
    -- }
    -- else {
    --     this.simulationValues.albedo_data.set(0, this.calcul_albedo(this.simulationValues.niveau_calottes_data.get(0)) * 100.);
    -- }
    -- ...
    -- if (!this.simulationValues.fixed_albedo) {
    --     CLogger.log('Method modelExecute: calcul_albedo: albedo_ter = ' + CPhysicsConstants.albedo_ter);
    --     zalbedo = this.calcul_albedo(zphig);
    if sv.fixed_albedo then
        sv.albedo_value / 100

    else if zphig > PhysicsConstants.phig_crit then
        ModelPhysicsConstants.albedo_crit
            + (zphig - PhysicsConstants.phig_crit)
            / (PhysicsConstants.niveau_calottes_max - PhysicsConstants.phig_crit)
            * (PhysicsConstants.albedo_ter - ModelPhysicsConstants.albedo_crit)

    else
        PhysicsConstants.albedo_glace_const
            + (zphig - PhysicsConstants.niveau_calottes_min)
            / (PhysicsConstants.phig_crit - PhysicsConstants.niveau_calottes_min)
            * (ModelPhysicsConstants.albedo_crit - PhysicsConstants.albedo_glace_const)


zpuit_bio : Config -> Float
zpuit_bio sv =
    if sv.fixed_concentration then
        -- undefined in SimClimat
        0

    else if sv.debranche_biologie then
        0

    else
        sv.puit_bio_value / 100


calcul_zpuit_oce : Config -> Float -> Float
calcul_zpuit_oce sv zT =
    if sv.debranche_ocean || sv.fixed_ocean then
        0

    else
        ((zT - PhysicsConstants.tcrit_oce - PhysicsConstants.tKelvin)
            / (PhysicsConstants.temperature_actuelle - PhysicsConstants.tcrit_oce)
            * PhysicsConstants.puit_ocean_act
            / 100
        )
            |> max 0
            |> min (PhysicsConstants.puit_oce_max / 100)


calcul_zB_ocean : Ancien -> Float
calcul_zB_ocean ancien =
    calcul_zC_alteration ancien.b_ocean ancien.zphig


calcul_zC_alteration : Float -> Float -> Float
calcul_zC_alteration cmax zphig =
    if zphig > PhysicsConstants.niveau_calotte_critique_coo then
        cmax

    else if zphig > 1.0e-2 then
        cmax * zphig / PhysicsConstants.niveau_calotte_critique_coo

    else
        0.0


calcul_zphig : Config -> Ancien -> Float -> Float
calcul_zphig sv ancien tau_niveau_calottes =
    calculT (calcul_phieq sv ancien.zT) ancien.zphig tau_niveau_calottes dt
        |> max 0
        |> min 90


calculT : Float -> Float -> Float -> Float -> Float
calculT tEq tPrec tau dt_ =
    tPrec + (tEq - tPrec) * (1 - exp (-dt_ / tau))


calcul_tau_niveau_calottes : Config -> Int -> Float
calcul_tau_niveau_calottes sv t =
    -- let
    --     -- FIXME
    --     zphig_ancien =
    --         50.0
    -- in
    -- if zphig_ancien < phieq sv t then
    --     PhysicsConstants.tau_niveau_calottes_deglacement
    -- else
    PhysicsConstants.tau_niveau_calottes_englacement


calcul_phieq : Config -> Float -> Float
calcul_phieq sv zT =
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


calcul_alteration_max : Config -> Float
calcul_alteration_max sv =
    ModelPhysicsConstants.c_alteration_naturel * (sv.alteration_value / 100.0)


calcul_b_ocean : Config -> Float
calcul_b_ocean sv =
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
    calcul_fin0 sv
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


calcul_fin0 : Config -> Float
calcul_fin0 sv =
    -- Fin0 = CPhysicsConstants.puissance_recue_zero * (this.simulationValues.puissance_soleil_value / 100.) / (this.simulationValues.distance_ts_value / 100) / (this.simulationValues.distance_ts_value / 100);
    PhysicsConstants.puissance_recue_zero
        * (sv.puissance_soleil_value / 100.0)
        / (sv.distance_ts_value / 100)
        / (sv.distance_ts_value / 100)


dt : Float
dt =
    EV.temps_elem ev / toFloat niter


niter : Int
niter =
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
