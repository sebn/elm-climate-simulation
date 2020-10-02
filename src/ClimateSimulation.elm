module ClimateSimulation exposing
    ( SimulationValues
    , fromSimClimat
    , simulate
    , toSimClimat
    )

import ClimateSimulation.Duration as Duration exposing (Duration)
import ClimateSimulation.Math exposing (exp, log)
import ClimateSimulation.Parameters as Parameters exposing (Parameters)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import ClimateSimulation.State as State exposing (State(..))
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import List.Nonempty as NEList


type alias SimulationValues =
    { name : String
    , parameters : Parameters
    , results : List State
    }


fromSimClimat : JD.Value -> Result JD.Error SimulationValues
fromSimClimat json =
    JD.decodeValue simulationValuesDecoder json


toSimClimat : SimulationValues -> JE.Value
toSimClimat simulation =
    JE.object <|
        List.concat
            [ [ ( "simulation_name", JE.string simulation.name )
              , ( "ID_MIN", JE.int 0 )
              , ( "ID_MAX", JE.int n )
              , ( "TEMPS_ELEM", JE.int 1 )
              , ( "INTERN_ECHEANCE", JE.int 100 )
              ]
            , Parameters.toSimClimatFields simulation.parameters
            , [ ( "temperature_data"
                , toSimClimatDataArray
                    { data = List.map State.temperature simulation.results
                    , pastData = Parameters.temperature_past_data simulation.parameters
                    }
                )
              , ( "concentrations_coo_data"
                , toSimClimatDataArray
                    { data = List.map State.co2Concentration simulation.results
                    , pastData = []
                    }
                )
              , ( "niveau_calottes_data"
                , toSimClimatDataArray
                    { data = List.map State.iceCap simulation.results
                    , pastData = []
                    }
                )
              , ( "emissions_coo_data"
                , toSimClimatDataArray
                    { data = emissions_coo_data simulation |> duplicateLast
                    , pastData = []
                    }
                )
              , ( "albedo_data"
                , toSimClimatDataArray
                    { data = albedo_data simulation
                    , pastData = []
                    }
                )
              , ( "niveau_mer_data"
                , toSimClimatDataArray
                    { data = niveau_mer_data simulation
                    , pastData = []
                    }
                )
              , ( "modelPhysicsConstants", PhysicsConstants.toSimClimat )
              , ( "modelVarsConstants"
                , JE.object
                    [ ( "modelConstants", simClimatModelConstants )
                    ]
                )
              ]
            ]


duplicateLast : List a -> List a
duplicateLast items =
    case items of
        [] ->
            items

        [ x ] ->
            [ x, x ]

        first :: rest ->
            first :: duplicateLast rest


albedo_data : SimulationValues -> List Float
albedo_data sv =
    List.map State.albedoPercentage sv.results


emissions_coo_data : SimulationValues -> List Float
emissions_coo_data sv =
    List.map2 (calcul_emission_coo sv)
        sv.results
        (sv.results |> List.tail |> Maybe.withDefault [])


calcul_emission_coo : SimulationValues -> State -> State -> Float
calcul_emission_coo sv previousState state =
    if sv.parameters.fixed_concentration then
        0

    else
        (State.co2Concentration state
            - State.co2Concentration previousState
        )
            / Duration.temps_elem sv.parameters.duration


simulationValuesDecoder : JD.Decoder SimulationValues
simulationValuesDecoder =
    JD.succeed SimulationValues
        |> JDP.required "simulation_name" JD.string
        |> JDP.custom Parameters.simClimatDecoder
        |> JDP.hardcoded []


toSimClimatDataArray : { data : List Float, pastData : List Float } -> JE.Value
toSimClimatDataArray { data, pastData } =
    JE.object
        [ ( "N", JE.int n )
        , ( "datas", JE.list JE.float data )
        , ( "past_datas", JE.list JE.float pastData )
        , ( "resolution", JE.int n )
        , ( "indice_min", JE.int 0 )
        , ( "indice_max", JE.int n )
        , ( "imin", JE.int 0 )
        , ( "imax", JE.int n )
        ]


n : Int
n =
    100


boucleT : SimulationValues -> List State
boucleT sv =
    let
        initialState =
            computeInitialState sv
    in
    List.range 1 n
        |> List.foldl
            (prependNextResult sv)
            (NEList.fromElement initialState)
        |> NEList.reverse
        |> NEList.tail
        |> (::) initialState


computeInitialState : SimulationValues -> State
computeInitialState sv =
    let
        zT0 =
            Parameters.zT0 sv.parameters

        zphig0 =
            Parameters.zphig0 sv.parameters

        zCO2 =
            sv.parameters.coo_concentr_value
    in
    State
        { alteration_max = calcul_alteration_max sv
        , b_ocean = calcul_b_ocean sv
        , fdegaz = 0
        , fin = calcul_fin0 sv
        , forcage_serre = 0
        , forcage_serre_CO2 = calcul_forcage_serre_CO2 zCO2
        , forcage_serre_eau = 0
        , g = 0
        , insol65N = insol65N sv
        , oscillation = 0
        , phieq = calcul_phieq sv zT0
        , tau_niveau_calottes = calcul_tau_niveau_calottes sv 0
        , zB_ocean = 0
        , zC_alteration = 0
        , zC_stockage = 0
        , zCO2 = zCO2
        , zCO2_prec = zCO2
        , zCO2eq_oce = 0
        , zT = zT0
        , zT_ancien = zT0
        , zTeq = 0
        , zalbedo = calcul_albedo sv zphig0
        , zphig = zphig0
        , zphig_ancien = zphig0
        , zpuit_bio = calcul_zpuit_bio sv
        , zpuit_oce = Parameters.zpuit_oce0 sv.parameters
        , zrapport_H2O = calcul_rapport_H2O sv zT0
        , zsomme_C = 0
        , zsomme_flux_const = 0
        }


prependNextResult : SimulationValues -> Int -> NEList.Nonempty State -> NEList.Nonempty State
prependNextResult sv t previousStates =
    let
        previousState =
            NEList.head previousStates

        nextState =
            computeNextResult sv t previousState
    in
    NEList.cons nextState previousStates


ev : Duration
ev =
    Duration.fromYears 10000


computeNextResult : SimulationValues -> Int -> State -> State
computeNextResult sv t (State previous) =
    List.range 1 niter
        |> List.foldl
            (computeNextIntermediateState sv t)
            (State { previous | oscillation = 0 })


computeNextIntermediateState : SimulationValues -> Int -> Int -> State -> State
computeNextIntermediateState sv t iter (State previousState) =
    let
        debug : String -> value -> value
        debug msg =
            -- if t == 1 && iter == 2 then
            --     Debug.log msg
            -- else
            identity

        tau_niveau_calottes =
            calcul_tau_niveau_calottes sv t

        zphig_raw =
            calcul_zphig sv (State previousState) tau_niveau_calottes

        zalbedo =
            calcul_albedo sv zphig_raw

        zC_alteration =
            calcul_zC_alteration previousState.alteration_max previousState.zphig

        zpuit_oce =
            calcul_zpuit_oce sv (State previousState)

        zC_stockage =
            calcul_zC_stockage sv previousState.zphig

        zB_ocean =
            calcul_zB_ocean (State previousState)

        zsomme_flux_const =
            calcul_zsomme_flux_const sv zpuit_oce previousState.zpuit_bio zB_ocean previousState.zT

        zsomme_C =
            calcul_zsomme_C zpuit_oce previousState.zpuit_bio zC_alteration zC_stockage zB_ocean

        zsomme_flux =
            zsomme_flux_const + previousState.zCO2 * zsomme_C

        emission_coo_ppm =
            calcul_emission_coo_ppm zsomme_flux

        zCO2_raw =
            calcul_zCO2 sv (State previousState) emission_coo_ppm dt

        zrapport_H2O =
            calcul_rapport_H2O sv previousState.zT

        forcage_serre_eau =
            calcul_forcage_serre_H2O zrapport_H2O

        forcage_serre_CO2 =
            calcul_forcage_serre_CO2 zCO2_raw

        forcage_serre =
            forcage_serre_CO2 + forcage_serre_eau

        g =
            calcul_G forcage_serre

        fin =
            calcul_fin sv zalbedo

        zTeq =
            calcul_zTeq fin g

        zT_raw =
            calcul_zT zTeq previousState.zT dt

        oscillation =
            calcul_oscillation sv iter (State previousState) zT_raw zCO2_raw

        zCO2 =
            if oscillation == 1 && not sv.parameters.fixed_concentration then
                (zCO2_raw + previousState.zCO2 + 0.5 * previousState.zCO2_prec) / 2.5

            else
                zCO2_raw

        zT =
            if oscillation == 1 then
                (zT_raw + previousState.zT + 0.5 * previousState.zT_ancien) / 2.5

            else
                zT_raw

        zphig =
            if oscillation == 1 then
                (zphig_raw + previousState.zphig + 0.5 * previousState.zphig_ancien) / 2.5

            else
                zphig_raw
    in
    State
        { previousState
            | fdegaz = calcul_fdegaz sv previousState.zT
            , fin = fin
            , forcage_serre = forcage_serre
            , forcage_serre_CO2 = forcage_serre_CO2
            , forcage_serre_eau = forcage_serre_eau
            , g = g
            , oscillation = oscillation
            , phieq = calcul_phieq sv previousState.zT
            , tau_niveau_calottes = tau_niveau_calottes
            , zB_ocean = zB_ocean
            , zC_alteration = zC_alteration
            , zC_stockage = zC_stockage
            , zCO2 = zCO2
            , zCO2_prec = previousState.zCO2
            , zCO2eq_oce = calcul_zCO2eq_oce previousState.zT
            , zT = zT
            , zT_ancien = previousState.zT
            , zTeq = zTeq
            , zalbedo = zalbedo
            , zphig = zphig
            , zphig_ancien = previousState.zphig
            , zpuit_oce = zpuit_oce
            , zrapport_H2O = zrapport_H2O
            , zsomme_C = zsomme_C
            , zsomme_flux_const = zsomme_flux_const
        }


niveau_mer_data : SimulationValues -> List Float
niveau_mer_data sv =
    List.indexedMap (calcul_niveau_mer sv) sv.results


calcul_niveau_mer : SimulationValues -> Int -> State -> Float
calcul_niveau_mer sv t (State { zphig, zT }) =
    if t == 0 then
        Parameters.niveau_mer0 sv.parameters

    else
        let
            -- tau_niveau_mer est en année, il faut diviser par temps_elem pour
            -- l'avoir en nombre de pas.
            --
            -- index doit pouvoir être soit positif, soit négatif. On met juste
            -- des bornes entre -100 et +100.
            index =
                (t - truncate (PhysicsConstants.tau_niveau_mer / Duration.temps_elem sv.parameters.duration))
                    |> min (Duration.indice_max sv.parameters.duration)
                    |> max -(Duration.indice_max sv.parameters.duration)

            tressentie =
                if index < 0 then
                    let
                        past_data =
                            sv.parameters
                                |> Parameters.temperature_past_data
                                |> List.drop index
                                -- get_past(-index) ???
                                |> List.head
                                |> Maybe.withDefault 0
                    in
                    zT * 0.2 + past_data * 0.8

                else
                    let
                        data =
                            sv.results
                                |> List.drop index
                                |> List.head
                                |> Maybe.andThen (Just << State.temperature)
                                |> Maybe.withDefault 0
                    in
                    zT * 0.2 + data * 0.8

            dilatation =
                PhysicsConstants.dilat
                    * 0.5
                    * (tressentie
                        - PhysicsConstants.tKelvin
                        - PhysicsConstants.tressentie_act
                      )

            hmer =
                PhysicsConstants.hmer_tot
                    * (1 + dilatation)
                    * (1
                        - PhysicsConstants.fphig1
                        * (zphig - PhysicsConstants.niveau_calottes_max)
                        - PhysicsConstants.fphig2
                        * ((zphig - PhysicsConstants.niveau_calottes_max) ^ 2)
                        - PhysicsConstants.fphig3
                        * ((zphig - PhysicsConstants.niveau_calottes_max) ^ 3)
                      )
        in
        hmer - PhysicsConstants.hmeract


calcul_oscillation : SimulationValues -> Int -> State -> Float -> Float -> Int
calcul_oscillation sv iter (State previous) zT zCO2 =
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
    if sv.parameters.fixed_concentration then
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


calcul_rapport_H2O : SimulationValues -> Float -> Float
calcul_rapport_H2O sv zT =
    if sv.parameters.fixed_eau then
        sv.parameters.rapport_H2O_value / 100

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


calcul_zCO2 : SimulationValues -> State -> Float -> Float -> Float
calcul_zCO2 sv previousState emission_coo_ppm dt_ =
    if sv.parameters.fixed_concentration then
        sv.parameters.coo_concentr_value

    else
        max 0 <|
            State.co2Concentration previousState
                + emission_coo_ppm
                * dt_


{-| ppm/an
-}
calcul_emission_coo_ppm : Float -> Float
calcul_emission_coo_ppm zsomme_flux =
    zsomme_flux * (PhysicsConstants.concentration_coo_actuel / PhysicsConstants.coo_Gt_act)


calcul_zsomme_flux_const : SimulationValues -> Float -> Float -> Float -> Float -> Float
calcul_zsomme_flux_const sv zpuit_oce zpuit_bio zB_ocean zT_ancien =
    max 0 (min 1 (1 - zpuit_oce - zpuit_bio))
        * (sv.parameters.emit_anthro_coo_value + sv.parameters.volcan_value)
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


calcul_fdegaz : SimulationValues -> Float -> Float
calcul_fdegaz sv zT =
    if sv.parameters.debranche_ocean || sv.parameters.fixed_ocean then
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


calcul_zC_stockage : SimulationValues -> Float -> Float
calcul_zC_stockage sv zphig_ancien =
    calcul_zC_alteration
        (-sv.parameters.stockage_biologique_value * 1.0e-3)
        zphig_ancien


calcul_fin : SimulationValues -> Float -> Float
calcul_fin sv zalbedo =
    calcul_fin0 sv * (1 - zalbedo)


calcul_albedo : SimulationValues -> Float -> Float
calcul_albedo sv zphig =
    if sv.parameters.fixed_albedo then
        sv.parameters.albedo_value / 100

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


calcul_zpuit_bio : SimulationValues -> Float
calcul_zpuit_bio sv =
    if sv.parameters.fixed_concentration then
        0

    else if sv.parameters.debranche_biologie then
        0

    else
        sv.parameters.puit_bio_value / 100


calcul_zpuit_oce : SimulationValues -> State -> Float
calcul_zpuit_oce sv (State { zT, zpuit_oce }) =
    if sv.parameters.fixed_concentration || sv.parameters.debranche_ocean || sv.parameters.fixed_ocean then
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
calcul_zB_ocean (State previousState) =
    calcul_zC_alteration previousState.b_ocean previousState.zphig


calcul_zC_alteration : Float -> Float -> Float
calcul_zC_alteration cmax zphig =
    if zphig > PhysicsConstants.niveau_calotte_critique_coo then
        cmax

    else if zphig > 1.0e-2 then
        cmax * zphig / PhysicsConstants.niveau_calotte_critique_coo

    else
        0.0


calcul_zphig : SimulationValues -> State -> Float -> Float
calcul_zphig sv (State previousState) tau_niveau_calottes =
    calculT (calcul_phieq sv previousState.zT) previousState.zphig tau_niveau_calottes dt
        |> max 0
        |> min 90


calculT : Float -> Float -> Float -> Float -> Float
calculT tEq tPrec tau dt_ =
    tPrec + (tEq - tPrec) * (1 - exp (-dt_ / tau))


calcul_tau_niveau_calottes : SimulationValues -> Int -> Float
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


calcul_phieq : SimulationValues -> Float -> Float
calcul_phieq sv zT =
    (PhysicsConstants.a_calottes
        * (zT - PhysicsConstants.tKelvin)
        + PhysicsConstants.b_calottes
        + PhysicsConstants.c_calottes
        * (insol65N sv
            - PhysicsConstants.insol_actuel
          )
    )
        |> min PhysicsConstants.niveau_calottes_max
        |> max PhysicsConstants.niveau_calottes_min


calcul_alteration_max : SimulationValues -> Float
calcul_alteration_max sv =
    PhysicsConstants.c_alteration_naturel * (sv.parameters.alteration_value / 100.0)


calcul_b_ocean : SimulationValues -> Float
calcul_b_ocean sv =
    if sv.parameters.debranche_ocean then
        0

    else if sv.parameters.fixed_ocean then
        0

    else
        PhysicsConstants.b_ocean


{-| Insolation 65º lat. N
-}
insol65N : SimulationValues -> Float
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
                            - (0.3 * sv.parameters.excentricite_value + 0.7 * PhysicsConstants.excentricite_actuel)
                            * 0.5
                            * sin (-sv.parameters.precession_value / 180 * pi)
                          )
                    )
            )


delta_angle : SimulationValues -> Float
delta_angle sv =
    abs
        ((toFloat PhysicsConstants.lat_Mil - sv.parameters.obliquite_value)
            / 360
            * 2
            * PhysicsConstants.pi
        )


calcul_fin0 : SimulationValues -> Float
calcul_fin0 sv =
    PhysicsConstants.puissance_recue_zero
        * (sv.parameters.puissance_soleil_value / 100.0)
        / (sv.parameters.distance_ts_value / 100)
        / (sv.parameters.distance_ts_value / 100)


dt : Float
dt =
    Duration.temps_elem ev / toFloat niter


niter : Int
niter =
    max 4 (truncate (3 * exp (0.3 * log (Duration.temps_elem ev))))


simulate : SimulationValues -> SimulationValues
simulate config =
    { config | results = boucleT config }


simClimatModelConstants : JE.Value
simClimatModelConstants =
    JE.object
        [ ( "enable_gl_animations", JE.float 0 )
        , ( "echeance_min", JE.float 100 )
        , ( "echeance_max", JE.float 10000000 )
        , ( "echeance_reset_value", JE.float 500 )
        , ( "time_slider_reset_value", JE.float 0 )
        , ( "graphe_one_init", JE.float 0 )
        , ( "graphe_two_init", JE.float 3 )
        , ( "graphe_three_init", JE.float 1 )
        , ( "graphe_four_init", JE.float 2 )
        , ( "fixed_concentration_reset_value", JE.float 1 )
        , ( "coo_concentr_terre_initiale", JE.float 300000 )
        , ( "coo_concentr_cretace", JE.float 1500 )
        , ( "coo_concentr_1750", JE.float 280 )
        , ( "coo_concentr_today", JE.float 405 )
        , ( "coo_concentr_min", JE.float 0 )
        , ( "coo_concentr_max", JE.float 1000000 )
        , ( "coo_concentr_reset_value", JE.float 280 )
        , ( "emit_anthro_coo_actuel", JE.float 8 )
        , ( "emit_anthro_coo_2xactuel", JE.float 16 )
        , ( "emit_anthro_coo_nul", JE.float 0 )
        , ( "emit_anthro_coo_min", JE.float -2 )
        , ( "emit_anthro_coo_max", JE.float 50 )
        , ( "emit_anthro_coo_reset_value", JE.float 0 )
        , ( "volcan_actuel", JE.float 0.083 )
        , ( "volcan_terre_init", JE.float 0.41500000000000004 )
        , ( "volcan_min", JE.float 0 )
        , ( "volcan_max", JE.float 2.49 )
        , ( "volcan_reset_value", JE.float 0.083 )
        , ( "alteration_actuel", JE.float 100 )
        , ( "alteration_min", JE.float 0 )
        , ( "alteration_max", JE.float 1000 )
        , ( "alteration_reset_value", JE.float 100 )
        , ( "stockage_biologique_actuel", JE.float 0 )
        , ( "stockage_biologique_carbonifere", JE.float 0.7071428571428572 )
        , ( "stockage_biologique_min", JE.float 0 )
        , ( "stockage_biologique_max", JE.float 35.714285714285715 )
        , ( "stockage_biologique_reset_value", JE.float 0 )
        , ( "fixed_albedo_reset_value", JE.float 0 )
        , ( "albedo_act", JE.float 32.99138091474444 )
        , ( "albedo_reset_value", JE.float 32.99138091474444 )
        , ( "albedo_glace", JE.float 90 )
        , ( "albedo_terre", JE.float 25 )
        , ( "albedo_min", JE.float 0 )
        , ( "albedo_max", JE.float 100 )
        , ( "debranche_biologie_reset_value", JE.float 0 )
        , ( "puit_bio_actuel", JE.float 35 )
        , ( "puit_bio_min", JE.float -50 )
        , ( "puit_bio_max", JE.float 100 )
        , ( "puit_bio_reset_value", JE.float 35 )
        , ( "fixed_ocean_reset_value", JE.float 1 )
        , ( "debranche_ocean_reset_value", JE.float 0 )
        , ( "puit_ocean_actuel", JE.float 20 )
        , ( "puit_ocean_min", JE.float -50 )
        , ( "puit_ocean_max", JE.float 100 )
        , ( "puit_ocean_reset_value", JE.float 20 )
        , ( "fixed_eau_reset_value", JE.float 0 )
        , ( "rapport_H2O_reset_value", JE.float 100 )
        , ( "rapport_H2O_actuel", JE.float 105.71900000000001 )
        , ( "rapport_H2O_min", JE.float 0 )
        , ( "rapport_H2O_max", JE.float 1000 )
        , ( "puissance_soleil_terre_init", JE.float 70 )
        , ( "puissance_soleil_actuel", JE.float 100 )
        , ( "puissance_soleil_min", JE.float 0 )
        , ( "puissance_soleil_max", JE.float 1000 )
        , ( "puissance_soleil_reset_value", JE.float 100 )
        , ( "distance_ts_actuel", JE.float 100 )
        , ( "distance_ts_min", JE.float 0 )
        , ( "distance_ts_max", JE.float 1000 )
        , ( "distance_ts_reset_value", JE.float 100 )
        , ( "excentricite_act", JE.float 0.0167 )
        , ( "excentricite_reset_value", JE.float 0.0167 )
        , ( "excentricite_valeur_min", JE.float 0 )
        , ( "excentricite_valeur_max", JE.float 0.06 )
        , ( "excentricite_autre_min", JE.float 0 )
        , ( "excentricite_autre_max", JE.float 0.2 )
        , ( "obliquite_act", JE.float 23.5 )
        , ( "obliquite_reset_value", JE.float 23.5 )
        , ( "obliquite_valeur_min", JE.float 21.8 )
        , ( "obliquite_valeur_max", JE.float 24.4 )
        , ( "obliquite_autre_min", JE.float 0 )
        , ( "obliquite_autre_max", JE.float 90 )
        , ( "precession_act", JE.float 102.7 )
        , ( "precession_reset_value", JE.float 102.7 )
        , ( "precession_valeur_min", JE.float 90 )
        , ( "precession_valeur_max", JE.float 270 )
        , ( "precession_autre_min", JE.float 0 )
        , ( "precession_autre_max", JE.float 360 )
        ]
