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
    , fdegaz : Float
    , fin : Float
    , forcage_serre : Float
    , forcage_serre_CO2 : Float
    , forcage_serre_eau : Float
    , g : Float
    , insol65N : Float
    , phieq : Float
    , tau_niveau_calottes : Float
    , zB_ocean : Float
    , zC_alteration : Float
    , zC_stockage : Float
    , zCO2 : Float
    , zCO2eq_oce : Float
    , zT : Float
    , zT_ancien : Float
    , zTeq : Float
    , zalbedo : Float
    , zphig : Float
    , zphig_ancien : Float
    , zpuit_bio : Float
    , zpuit_oce : Float
    , zrapport_H2O : Float
    , zsomme_C : Float
    , zsomme_flux_const : Float
    }


boucleT : Config -> List Ancien
boucleT sv =
    let
        zT0 =
            calcul_zT0 sv

        zphig0 =
            calcul_zphig0 sv

        zCO2 =
            sv.coo_concentr_value

        ancien =
            { alteration_max = calcul_alteration_max sv
            , b_ocean = calcul_b_ocean sv
            , fdegaz = 0
            , fin = calcul_fin0 sv
            , forcage_serre = 0
            , forcage_serre_CO2 = calcul_forcage_serre_CO2 zCO2
            , forcage_serre_eau = 0
            , g = 0
            , insol65N = insol65N sv
            , phieq = calcul_phieq sv zT0
            , tau_niveau_calottes = calcul_tau_niveau_calottes sv 0
            , zB_ocean = 0
            , zC_alteration = 0
            , zC_stockage = 0
            , zCO2 = zCO2
            , zCO2eq_oce = 0
            , zT = zT0
            , zT_ancien = zT0
            , zTeq = 0
            , zalbedo = 0
            , zphig = zphig0
            , zphig_ancien = zphig0
            , zpuit_bio = calcul_zpuit_bio sv
            , zpuit_oce = 0
            , zrapport_H2O = calcul_rapport_H2O sv zT0
            , zsomme_C = 0
            , zsomme_flux_const = 0
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
        tau_niveau_calottes =
            calcul_tau_niveau_calottes sv t

        zphig =
            calcul_zphig sv ancien tau_niveau_calottes

        zalbedo =
            calcul_albedo sv zphig

        zC_alteration =
            calcul_zC_alteration ancien.alteration_max ancien.zphig

        zpuit_oce =
            calcul_zpuit_oce sv ancien.zT

        zC_stockage =
            calcul_zC_stockage sv ancien.zphig

        zB_ocean =
            calcul_zB_ocean ancien

        zsomme_flux_const =
            calcul_zsomme_flux_const sv zpuit_oce ancien.zpuit_bio zB_ocean ancien.zT

        zsomme_C =
            calcul_zsomme_C zpuit_oce ancien.zpuit_bio zC_alteration zC_stockage zB_ocean

        zsomme_flux =
            zsomme_flux_const + ancien.zCO2 * zsomme_C

        emission_coo_ppm =
            calcul_emission_coo_ppm zsomme_flux

        zCO2 =
            calcul_zCO2 ancien emission_coo_ppm dt

        zrapport_H2O =
            calcul_rapport_H2O sv ancien.zT

        forcage_serre_eau =
            calcul_forcage_serre_H2O zrapport_H2O

        forcage_serre_CO2 =
            calcul_forcage_serre_CO2 zCO2

        forcage_serre =
            forcage_serre_CO2 + forcage_serre_eau

        g =
            calcul_G forcage_serre

        fin =
            calcul_fin sv zalbedo

        zTeq =
            calcul_zTeq fin g

        zT =
            calcul_zT zTeq ancien.zT dt
    in
    { ancien
        | fdegaz = calcul_fdegaz sv ancien.zT
        , fin = fin
        , forcage_serre = forcage_serre
        , forcage_serre_CO2 = forcage_serre_CO2
        , forcage_serre_eau = forcage_serre_eau
        , g = g
        , phieq = calcul_phieq sv ancien.zT
        , tau_niveau_calottes = tau_niveau_calottes
        , zB_ocean = zB_ocean
        , zC_alteration = zC_alteration
        , zC_stockage = zC_stockage
        , zCO2 = zCO2
        , zCO2eq_oce = calcul_zCO2eq_oce ancien.zT
        , zT = zT
        , zT_ancien = ancien.zT
        , zTeq = zTeq
        , zalbedo = zalbedo
        , zphig = zphig
        , zphig_ancien = ancien.zphig
        , zpuit_oce = zpuit_oce
        , zrapport_H2O = zrapport_H2O
        , zsomme_C = zsomme_C
        , zsomme_flux_const = zsomme_flux_const
    }


calcul_zT : Float -> Float -> Float -> Float
calcul_zT zTeq zT_ancien dt_ =
    calculT zTeq zT_ancien PhysicsConstants.tau_temperature dt_
        |> min 1.0e6
        |> max 0


calcul_zTeq : Float -> Float -> Float
calcul_zTeq fin g =
    exp (0.25 * log (fin / (g * PhysicsConstants.sigma)))


calcul_G : Float -> Float
calcul_G forcage_serre =
    ModelPhysicsConstants.g0
        - forcage_serre
        |> max PhysicsConstants.g_min
        |> min 1


calcul_forcage_serre_CO2 : Float -> Float
calcul_forcage_serre_CO2 zCO2 =
    if zCO2 < PhysicsConstants.concentration_coo_limite_bas then
        -- relation linéaire, extrapolation basse
        -PhysicsConstants.q_CO2
            * ModelPhysicsConstants.g0
            + zCO2
            / PhysicsConstants.concentration_coo_limite_bas
            * (PhysicsConstants.q_CO2
                * ModelPhysicsConstants.g0
                + PhysicsConstants.a_coo
                * log (PhysicsConstants.concentration_coo_limite_bas / PhysicsConstants.concentration_coo_1750)
              )

    else if zCO2 > PhysicsConstants.concentration_coo_limite then
        -- relation linéaire, extrapolation haute
        PhysicsConstants.a_coo
            * (log
                (PhysicsConstants.concentration_coo_limite
                    / PhysicsConstants.concentration_coo_1750
                )
                + 1
                * (zCO2 / PhysicsConstants.concentration_coo_limite - 1)
              )

    else
        -- relation log
        PhysicsConstants.a_coo
            * log (zCO2 / PhysicsConstants.concentration_coo_1750)


calcul_forcage_serre_H2O : Float -> Float
calcul_forcage_serre_H2O zrapport_H2O =
    if zrapport_H2O > 1.0e-5 then
        ModelPhysicsConstants.a_H2O
            * (1
                - exp (PhysicsConstants.pow_H2O * log zrapport_H2O)
              )
            * (0.3
                * exp
                    (-(sqrt (abs (zrapport_H2O - 1)))
                        / 10.0
                    )
                + 0.7
              )

    else
        ModelPhysicsConstants.a_H2O


calcul_rapport_H2O : Config -> Float -> Float
calcul_rapport_H2O sv zT =
    if sv.fixed_eau then
        sv.rapport_H2O_value / 100

    else
        -- utilisation de la formule de Rankine donnant Psat
        exp
            (PhysicsConstants.a_rankine
                - PhysicsConstants.b_rankine
                / zT
            )
            / exp
                (PhysicsConstants.a_rankine
                    - PhysicsConstants.b_rankine
                    / (PhysicsConstants.temperature_1750 + PhysicsConstants.tKelvin)
                )


calcul_zCO2 : Ancien -> Float -> Float -> Float
calcul_zCO2 ancien emission_coo_ppm dt_ =
    max 0 <|
        ancien.zCO2
            + emission_coo_ppm
            * dt_


{-| ppm/an
-}
calcul_emission_coo_ppm zsomme_flux =
    zsomme_flux * (PhysicsConstants.concentration_coo_actuel / PhysicsConstants.coo_Gt_act)


calcul_zsomme_flux_const sv zpuit_oce zpuit_bio zB_ocean zT_ancien =
    max 0 (min 1 (1 - zpuit_oce - zpuit_bio))
        * (sv.emit_anthro_coo_value + sv.volcan_value)
        + max 0 (min 1 (1 - zpuit_bio))
        * zB_ocean
        * calcul_zCO2eq_oce zT_ancien
        + calcul_fdegaz sv zT_ancien


calcul_zCO2eq_oce : Float -> Float
calcul_zCO2eq_oce zT =
    let
        c =
            15

        b =
            (PhysicsConstants.concentration_coo_glaciaire - PhysicsConstants.concentration_coo_1750)
                / (atan (PhysicsConstants.temperature_LGM - c)
                    - atan (PhysicsConstants.temperature_1750 - c)
                  )

        a =
            PhysicsConstants.concentration_coo_glaciaire
                - b
                * atan (PhysicsConstants.temperature_LGM - c)
    in
    a
        + b
        * atan (zT - PhysicsConstants.tKelvin - c)
        + max 0 (5 * (zT - PhysicsConstants.tKelvin - c))


calcul_fdegaz : Config -> Float -> Float
calcul_fdegaz sv zT =
    if sv.debranche_ocean || sv.fixed_ocean then
        0

    else if zT - PhysicsConstants.tKelvin > PhysicsConstants.tcrit_oce then
        (zT - PhysicsConstants.tcrit_oce - PhysicsConstants.tKelvin) * PhysicsConstants.dFdegaz

    else
        0


calcul_zsomme_C : Float -> Float -> Float -> Float -> Float -> Float
calcul_zsomme_C zpuit_oce zpuit_bio zC_alteration zC_stockage zB_ocean =
    max 0 (min 1 (1 - zpuit_oce - zpuit_bio))
        * (zC_alteration + zC_stockage)
        - max 0 (min 1 (1 - zpuit_bio))
        * zB_ocean


calcul_zC_stockage : Config -> Float -> Float
calcul_zC_stockage sv zphig_ancien =
    calcul_zC_alteration
        (-sv.stockage_biologique_value * 1.0e-3)
        zphig_ancien


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


calcul_zpuit_bio : Config -> Float
calcul_zpuit_bio sv =
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
