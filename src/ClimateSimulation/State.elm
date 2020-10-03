module ClimateSimulation.State exposing
    ( State
    , albedoPercentage
    , celsiusTemperature
    , co2Concentration
    , iceCap
    , initial
    , next
    , temperature
    , timeInYears
    , toJson
    )

import ClimateSimulation.Duration as Duration
import ClimateSimulation.Math exposing (exp, log)
import ClimateSimulation.Parameters as Parameters exposing (Parameters)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import Json.Encode as JE


type State
    = State
        { alteration_max : Float
        , b_ocean : Float
        , fdegaz : Float
        , fin : Float
        , forcage_serre : Float
        , forcage_serre_CO2 : Float
        , forcage_serre_eau : Float
        , g : Float
        , insol65N : Float
        , oscillation : Int
        , phieq : Float
        , tau_niveau_calottes : Float
        , zB_ocean : Float
        , zC_alteration : Float
        , zC_stockage : Float
        , zCO2 : Float
        , zCO2_prec : Float
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
        , timeInYears : Float
        }


albedoPercentage : State -> Float
albedoPercentage (State { zalbedo }) =
    zalbedo * 100


co2Concentration : State -> Float
co2Concentration (State { zCO2 }) =
    zCO2


iceCap : State -> Float
iceCap (State { zphig }) =
    zphig


temperature : State -> Float
temperature (State { zT }) =
    zT


celsiusTemperature : State -> Float
celsiusTemperature state =
    temperature state - PhysicsConstants.tKelvin


timeInYears : State -> Float
timeInYears (State state) =
    state.timeInYears



-- INITIAL STATE


initial : Parameters -> State
initial parameters =
    let
        zT0 =
            Parameters.zT0 parameters

        zphig0 =
            Parameters.zphig0 parameters

        zCO2 =
            parameters.coo_concentr_value

        phieq0 =
            calcul_phieq parameters zT0
    in
    State
        { alteration_max = Parameters.alteration_max parameters
        , b_ocean = Parameters.b_ocean parameters
        , fdegaz = 0
        , fin = Parameters.fin0 parameters
        , forcage_serre = 0
        , forcage_serre_CO2 = calcul_forcage_serre_CO2 zCO2
        , forcage_serre_eau = 0
        , g = 0
        , insol65N = Parameters.insol65N parameters
        , oscillation = 0
        , phieq = phieq0
        , tau_niveau_calottes = calcul_tau_niveau_calottes phieq0 zphig0
        , zB_ocean = 0
        , zC_alteration = 0
        , zC_stockage = 0
        , zCO2 = zCO2
        , zCO2_prec = zCO2
        , zCO2eq_oce = 0
        , zT = zT0
        , zT_ancien = zT0
        , zTeq = 0
        , zalbedo = calcul_albedo parameters zphig0
        , zphig = zphig0
        , zphig_ancien = zphig0
        , zpuit_bio = Parameters.zpuit_bio parameters
        , zpuit_oce = Parameters.zpuit_oce0 parameters
        , zrapport_H2O = calcul_rapport_H2O parameters zT0
        , zsomme_C = 0
        , zsomme_flux_const = 0
        , timeInYears = toFloat <| Parameters.startYear parameters.initialState
        }



-- NEXT STATE


next : Parameters -> Int -> State -> State
next parameters t (State previous) =
    List.range 1 (Duration.niter parameters.duration)
        |> List.foldl
            (nextIntermediate parameters)
            (State
                { previous
                    | oscillation = 0
                    , timeInYears = toFloat <| t + Parameters.startYear parameters.initialState
                }
            )


nextIntermediate : Parameters -> Int -> State -> State
nextIntermediate parameters iter (State previous) =
    let
        dt =
            Duration.dt parameters.duration

        phieq =
            calcul_phieq parameters previous.zT

        tau_niveau_calottes =
            calcul_tau_niveau_calottes phieq previous.zphig

        zphig_raw =
            calcul_zphig parameters (State previous) tau_niveau_calottes

        zalbedo =
            calcul_albedo parameters zphig_raw

        zC_alteration =
            calcul_zC_alteration previous.alteration_max previous.zphig

        zpuit_oce =
            calcul_zpuit_oce parameters (State previous)

        zC_stockage =
            calcul_zC_stockage parameters previous.zphig

        zB_ocean =
            calcul_zB_ocean (State previous)

        zsomme_flux_const =
            calcul_zsomme_flux_const parameters zpuit_oce previous.zpuit_bio zB_ocean previous.zT

        zsomme_C =
            calcul_zsomme_C zpuit_oce previous.zpuit_bio zC_alteration zC_stockage zB_ocean

        zsomme_flux =
            zsomme_flux_const + previous.zCO2 * zsomme_C

        emission_coo_ppm =
            calcul_emission_coo_ppm zsomme_flux

        zCO2_raw =
            calcul_zCO2 parameters (State previous) emission_coo_ppm dt

        zrapport_H2O =
            calcul_rapport_H2O parameters previous.zT

        forcage_serre_eau =
            calcul_forcage_serre_H2O zrapport_H2O

        forcage_serre_CO2 =
            calcul_forcage_serre_CO2 zCO2_raw

        forcage_serre =
            forcage_serre_CO2 + forcage_serre_eau

        g =
            calcul_G forcage_serre

        fin =
            calcul_fin parameters zalbedo

        zTeq =
            calcul_zTeq fin g

        zT_raw =
            calcul_zT zTeq previous.zT dt

        oscillation =
            calcul_oscillation parameters iter (State previous) zT_raw zCO2_raw

        zCO2 =
            if oscillation == 1 && not parameters.fixed_concentration then
                (zCO2_raw + previous.zCO2 + 0.5 * previous.zCO2_prec) / 2.5

            else
                zCO2_raw

        zT =
            if oscillation == 1 then
                (zT_raw + previous.zT + 0.5 * previous.zT_ancien) / 2.5

            else
                zT_raw

        zphig =
            if oscillation == 1 then
                (zphig_raw + previous.zphig + 0.5 * previous.zphig_ancien) / 2.5

            else
                zphig_raw
    in
    State
        { previous
            | fdegaz = calcul_fdegaz parameters previous.zT
            , fin = fin
            , forcage_serre = forcage_serre
            , forcage_serre_CO2 = forcage_serre_CO2
            , forcage_serre_eau = forcage_serre_eau
            , g = g
            , oscillation = oscillation
            , phieq = phieq
            , tau_niveau_calottes = tau_niveau_calottes
            , zB_ocean = zB_ocean
            , zC_alteration = zC_alteration
            , zC_stockage = zC_stockage
            , zCO2 = zCO2
            , zCO2_prec = previous.zCO2
            , zCO2eq_oce = calcul_zCO2eq_oce previous.zT
            , zT = zT
            , zT_ancien = previous.zT
            , zTeq = zTeq
            , zalbedo = zalbedo
            , zphig = zphig
            , zphig_ancien = previous.zphig
            , zpuit_oce = zpuit_oce
            , zrapport_H2O = zrapport_H2O
            , zsomme_C = zsomme_C
            , zsomme_flux_const = zsomme_flux_const
        }


calcul_oscillation : Parameters -> Int -> State -> Float -> Float -> Int
calcul_oscillation parameters iter (State previous) zT zCO2 =
    let
        oscillation =
            if (iter >= 3) && (previous.oscillation == 0) then
                -- pour éviter de détecter des oscillations à cause d'erreurs numériques
                -- on rajoute marge de 1e-5
                if previous.zT < previous.zT_ancien - 1.0e-5 then
                    if zT > previous.zT + 1.0e-5 then
                        1

                    else
                        previous.oscillation

                else if previous.zT > previous.zT_ancien + 1.0e-5 then
                    if zT < previous.zT - 1.0e-5 then
                        1

                    else
                        previous.oscillation

                else
                    previous.oscillation

            else
                previous.oscillation
    in
    if parameters.fixed_concentration then
        oscillation

    else if (iter >= 3) && (oscillation == 0) then
        -- pour éviter de détecter des oscillations à cause d'erreurs numériques
        -- on rajoute marge de 1e-5
        if previous.zCO2 < previous.zCO2_prec - 1.0e-5 then
            if zCO2 > previous.zCO2 + 1.0e-5 then
                1

            else
                oscillation

        else if previous.zCO2 > previous.zCO2_prec + 1.0e-5 then
            if zCO2 < previous.zCO2 - 1.0e-5 then
                1

            else
                oscillation

        else
            oscillation

    else
        oscillation


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
    PhysicsConstants.g0
        - forcage_serre
        |> max PhysicsConstants.g_min
        |> min 1


calcul_forcage_serre_CO2 : Float -> Float
calcul_forcage_serre_CO2 zCO2 =
    if zCO2 < PhysicsConstants.concentration_coo_limite_bas then
        -- relation linéaire, extrapolation basse
        -PhysicsConstants.q_CO2
            * PhysicsConstants.g0
            + zCO2
            / PhysicsConstants.concentration_coo_limite_bas
            * (PhysicsConstants.q_CO2
                * PhysicsConstants.g0
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
        PhysicsConstants.a_H2O
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
        PhysicsConstants.a_H2O


calcul_rapport_H2O : Parameters -> Float -> Float
calcul_rapport_H2O parameters zT =
    if parameters.fixed_eau then
        parameters.rapport_H2O_value / 100

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


calcul_zCO2 : Parameters -> State -> Float -> Float -> Float
calcul_zCO2 parameters (State previous) emission_coo_ppm dt_ =
    if parameters.fixed_concentration then
        parameters.coo_concentr_value

    else
        max 0 <|
            previous.zCO2
                + emission_coo_ppm
                * dt_


{-| ppm/an
-}
calcul_emission_coo_ppm : Float -> Float
calcul_emission_coo_ppm zsomme_flux =
    zsomme_flux * (PhysicsConstants.concentration_coo_actuel / PhysicsConstants.coo_Gt_act)


calcul_zsomme_flux_const : Parameters -> Float -> Float -> Float -> Float -> Float
calcul_zsomme_flux_const parameters zpuit_oce zpuit_bio zB_ocean zT_ancien =
    max 0 (min 1 (1 - zpuit_oce - zpuit_bio))
        * (parameters.emit_anthro_coo_value + parameters.volcan_value)
        + max 0 (min 1 (1 - zpuit_bio))
        * zB_ocean
        * calcul_zCO2eq_oce zT_ancien
        + calcul_fdegaz parameters zT_ancien


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


calcul_fdegaz : Parameters -> Float -> Float
calcul_fdegaz parameters zT =
    if parameters.debranche_ocean || parameters.fixed_ocean then
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


calcul_zC_stockage : Parameters -> Float -> Float
calcul_zC_stockage parameters zphig_ancien =
    calcul_zC_alteration
        (-parameters.stockage_biologique_value * 1.0e-3)
        zphig_ancien


calcul_fin : Parameters -> Float -> Float
calcul_fin parameters zalbedo =
    Parameters.fin0 parameters * (1 - zalbedo)


calcul_albedo : Parameters -> Float -> Float
calcul_albedo parameters zphig =
    if parameters.fixed_albedo then
        parameters.albedo_value / 100

    else if zphig > PhysicsConstants.phig_crit then
        PhysicsConstants.albedo_crit
            + (zphig - PhysicsConstants.phig_crit)
            / (PhysicsConstants.niveau_calottes_max - PhysicsConstants.phig_crit)
            * (PhysicsConstants.albedo_ter - PhysicsConstants.albedo_crit)

    else
        PhysicsConstants.albedo_glace_const
            + (zphig - PhysicsConstants.niveau_calottes_min)
            / (PhysicsConstants.phig_crit - PhysicsConstants.niveau_calottes_min)
            * (PhysicsConstants.albedo_crit - PhysicsConstants.albedo_glace_const)


calcul_zpuit_oce : Parameters -> State -> Float
calcul_zpuit_oce parameters (State { zT, zpuit_oce }) =
    if parameters.fixed_concentration || parameters.debranche_ocean || parameters.fixed_ocean then
        zpuit_oce

    else
        ((zT - PhysicsConstants.tcrit_oce - PhysicsConstants.tKelvin)
            / (PhysicsConstants.temperature_actuelle - PhysicsConstants.tcrit_oce)
            * PhysicsConstants.puit_ocean_act
            / 100
        )
            |> max 0
            |> min (PhysicsConstants.puit_oce_max / 100)


calcul_zB_ocean : State -> Float
calcul_zB_ocean (State previous) =
    calcul_zC_alteration previous.b_ocean previous.zphig


calcul_zC_alteration : Float -> Float -> Float
calcul_zC_alteration cmax zphig =
    if zphig > PhysicsConstants.niveau_calotte_critique_coo then
        cmax

    else if zphig > 1.0e-2 then
        cmax * zphig / PhysicsConstants.niveau_calotte_critique_coo

    else
        0.0


calcul_zphig : Parameters -> State -> Float -> Float
calcul_zphig parameters (State previous) tau_niveau_calottes =
    calculT (calcul_phieq parameters previous.zT) previous.zphig tau_niveau_calottes (Duration.dt parameters.duration)
        |> max 0
        |> min 90


calculT : Float -> Float -> Float -> Float -> Float
calculT tEq tPrec tau dt_ =
    tPrec + (tEq - tPrec) * (1 - exp (-dt_ / tau))


calcul_tau_niveau_calottes : Float -> Float -> Float
calcul_tau_niveau_calottes phieq zphig_ancien =
    if zphig_ancien < phieq then
        PhysicsConstants.tau_niveau_calottes_deglacement

    else
        PhysicsConstants.tau_niveau_calottes_englacement


calcul_phieq : Parameters -> Float -> Float
calcul_phieq parameters zT =
    (PhysicsConstants.a_calottes
        * (zT - PhysicsConstants.tKelvin)
        + PhysicsConstants.b_calottes
        + PhysicsConstants.c_calottes
        * (Parameters.insol65N parameters
            - PhysicsConstants.insol_actuel
          )
    )
        |> min PhysicsConstants.niveau_calottes_max
        |> max PhysicsConstants.niveau_calottes_min



-- IMPORT / EXPORT


toJson : State -> JE.Value
toJson (State state) =
    JE.object
        [ ( "alteration_max", JE.float state.alteration_max )
        , ( "b_ocean", JE.float state.b_ocean )
        , ( "fdegaz", JE.float state.fdegaz )
        , ( "fin", JE.float state.fin )
        , ( "forcage_serre", JE.float state.forcage_serre )
        , ( "forcage_serre_CO2", JE.float state.forcage_serre_CO2 )
        , ( "forcage_serre_eau", JE.float state.forcage_serre_eau )
        , ( "g", JE.float state.g )
        , ( "insol65N", JE.float state.insol65N )
        , ( "oscillation", JE.int state.oscillation )
        , ( "phieq", JE.float state.phieq )
        , ( "tau_niveau_calottes", JE.float state.tau_niveau_calottes )
        , ( "zB_ocean", JE.float state.zB_ocean )
        , ( "zC_alteration", JE.float state.zC_alteration )
        , ( "zC_stockage", JE.float state.zC_stockage )
        , ( "zCO2", JE.float state.zCO2 )

        -- , ( "zCO2_prec", JE.float state.zCO2_prec )
        , ( "zCO2eq_oce", JE.float state.zCO2eq_oce )
        , ( "zT", JE.float state.zT )
        , ( "zT_ancien", JE.float state.zT_ancien )
        , ( "zTeq", JE.float state.zTeq )
        , ( "zalbedo", JE.float state.zalbedo )
        , ( "zphig", JE.float state.zphig )
        , ( "zphig_ancien", JE.float state.zphig_ancien )
        , ( "zpuit_bio", JE.float state.zpuit_bio )
        , ( "zpuit_oce", JE.float state.zpuit_oce )
        , ( "zrapport_H2O", JE.float state.zrapport_H2O )
        , ( "zsomme_C", JE.float state.zsomme_C )
        , ( "zsomme_flux_const", JE.float state.zsomme_flux_const )
        ]
