module ClimateSimulation exposing
    ( SimulationValues
    , fromSimClimat
    , simulate
    , toSimClimat
    )

import ClimateSimulation.ExperienceValues as EV exposing (ExperienceValues)
import ClimateSimulation.Math exposing (exp, log)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import List.Nonempty as NEList


type alias SimulationValues =
    { -- CONFIG
      initialState : InitialState
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
    , results : List State
    }


fromSimClimat : JD.Value -> Result JD.Error SimulationValues
fromSimClimat json =
    JD.decodeValue simulationValuesDecoder json


toSimClimat : SimulationValues -> JE.Value
toSimClimat sv =
    JE.object
        [ ( "annee_debut", JE.int (startYear sv.initialState) )
        , ( "fixed_eau", JE.bool sv.fixed_eau )
        , ( "fixed_concentration", JE.bool sv.fixed_concentration )
        , ( "debranche_biologie", JE.bool sv.debranche_biologie )
        , ( "fixed_ocean", JE.bool sv.fixed_ocean )
        , ( "debranche_ocean", JE.bool sv.debranche_ocean )
        , ( "fixed_albedo", JE.bool sv.fixed_albedo )
        , ( "rapport_H2O_value", JE.float sv.rapport_H2O_value )
        , ( "puit_bio_value", JE.float sv.puit_bio_value )
        , ( "puit_oce_value", JE.float sv.puit_oce_value )
        , ( "albedo_value", JE.float sv.albedo_value )
        , ( "coo_concentr_value", JE.float sv.coo_concentr_value )
        , ( "puissance_soleil_value", JE.float sv.puissance_soleil_value )
        , ( "distance_ts_value", JE.float sv.distance_ts_value )
        , ( "obliquite_value", JE.float sv.obliquite_value )
        , ( "excentricite_value", JE.float sv.excentricite_value )
        , ( "precession_value", JE.float sv.precession_value )
        , ( "alteration_value", JE.float sv.alteration_value )
        , ( "emit_anthro_coo_value", JE.float sv.emit_anthro_coo_value )
        , ( "volcan_value", JE.float sv.volcan_value )
        , ( "stockage_biologique_value", JE.float sv.stockage_biologique_value )
        , ( "temperature_data"
          , toSimClimatDataArray
                { data = List.map .zT sv.results
                , pastData = temperature_past_data sv.initialState
                }
          )
        ]


simulationValuesDecoder : JD.Decoder SimulationValues
simulationValuesDecoder =
    JD.succeed SimulationValues
        |> JDP.required "annee_debut" initialStateDecoder
        |> JDP.required "fixed_eau" JD.bool
        |> JDP.required "fixed_concentration" JD.bool
        |> JDP.required "debranche_biologie" JD.bool
        |> JDP.required "fixed_ocean" JD.bool
        |> JDP.required "debranche_ocean" JD.bool
        |> JDP.required "fixed_albedo" JD.bool
        |> JDP.required "rapport_H2O_value" JD.float
        |> JDP.required "puit_bio_value" JD.float
        |> JDP.required "puit_oce_value" JD.float
        |> JDP.required "albedo_value" JD.float
        |> JDP.required "coo_concentr_value" JD.float
        |> JDP.required "puissance_soleil_value" JD.float
        |> JDP.required "distance_ts_value" JD.float
        |> JDP.required "obliquite_value" JD.float
        |> JDP.required "excentricite_value" JD.float
        |> JDP.required "precession_value" JD.float
        |> JDP.required "alteration_value" JD.float
        |> JDP.required "emit_anthro_coo_value" JD.float
        |> JDP.required "volcan_value" JD.float
        |> JDP.required "stockage_biologique_value" JD.float
        |> JDP.hardcoded []



-- INITIAL STATE


type InitialState
    = Now
    | PreIndustrial


startYear : InitialState -> Basics.Int
startYear initialState =
    case initialState of
        PreIndustrial ->
            1750

        Now ->
            2007


initialStateDecoder : JD.Decoder InitialState
initialStateDecoder =
    JD.float |> JD.andThen (truncate >> initialStateDecoderFromYear)


initialStateDecoderFromYear : Int -> JD.Decoder InitialState
initialStateDecoderFromYear year =
    case year of
        1750 ->
            JD.succeed PreIndustrial

        2007 ->
            JD.succeed Now

        _ ->
            -- FIXME
            JD.succeed Now


toSimClimatDataArray : { data : List Float, pastData : List Float } -> JE.Value
toSimClimatDataArray { data, pastData } =
    JE.object
        [ ( "datas", JE.list JE.float data )
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


stateToSimClimat : State -> JE.Value
stateToSimClimat state =
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


type alias State =
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
    }


boucleT : SimulationValues -> List State
boucleT sv =
    let
        zT0 =
            calcul_zT0 sv

        zphig0 =
            calcul_zphig0 sv

        zCO2 =
            sv.coo_concentr_value

        initialState =
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
            (prependNextResult sv)
            (NEList.fromElement initialState)
        |> NEList.reverse
        |> NEList.tail
        |> (::) initialState


prependNextResult : SimulationValues -> Int -> NEList.Nonempty State -> NEList.Nonempty State
prependNextResult sv t previousStates =
    let
        previousState =
            NEList.head previousStates

        nextState =
            computeNextResult sv t previousState
    in
    NEList.cons nextState previousStates


ev : ExperienceValues
ev =
    EV.fromEcheance 10000


calcul_zT0 : SimulationValues -> Float
calcul_zT0 sv =
    (+) PhysicsConstants.tKelvin <|
        case sv.initialState of
            PreIndustrial ->
                PhysicsConstants.temperature_1750

            _ ->
                PhysicsConstants.temperature_actuelle


calcul_zphig0 : SimulationValues -> Float
calcul_zphig0 sv =
    case sv.initialState of
        PreIndustrial ->
            PhysicsConstants.niveau_calottes_1750

        _ ->
            PhysicsConstants.niveau_calottes_actuel


zpuit_oce0 : SimulationValues -> Float
zpuit_oce0 sv =
    if sv.fixed_concentration then
        -- undefined in SimClimat
        0

    else if sv.debranche_ocean then
        0

    else
        sv.puit_oce_value / 100


computeNextResult : SimulationValues -> Int -> State -> State
computeNextResult sv t previous =
    List.range 1 niter
        |> List.foldl
            (computeNextIntermediateState sv t)
            { previous | oscillation = 0 }


computeNextIntermediateState : SimulationValues -> Int -> Int -> State -> State
computeNextIntermediateState sv t iter previousState =
    let
        tau_niveau_calottes =
            calcul_tau_niveau_calottes sv t

        zphig =
            calcul_zphig sv previousState tau_niveau_calottes

        zalbedo =
            calcul_albedo sv zphig

        zC_alteration =
            calcul_zC_alteration previousState.alteration_max previousState.zphig

        zpuit_oce =
            calcul_zpuit_oce sv previousState.zT

        zC_stockage =
            calcul_zC_stockage sv previousState.zphig

        zB_ocean =
            calcul_zB_ocean previousState

        zsomme_flux_const =
            calcul_zsomme_flux_const sv zpuit_oce previousState.zpuit_bio zB_ocean previousState.zT

        zsomme_C =
            calcul_zsomme_C zpuit_oce previousState.zpuit_bio zC_alteration zC_stockage zB_ocean

        zsomme_flux =
            zsomme_flux_const + previousState.zCO2 * zsomme_C

        emission_coo_ppm =
            calcul_emission_coo_ppm zsomme_flux

        zCO2 =
            calcul_zCO2 previousState emission_coo_ppm dt

        zrapport_H2O =
            calcul_rapport_H2O sv previousState.zT

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
            calcul_zT zTeq previousState.zT dt

        oscillation =
            calcul_oscillation sv iter previousState zT zCO2
    in
    { previousState
        | fdegaz = calcul_fdegaz sv previousState.zT
        , fin = fin
        , forcage_serre = forcage_serre
        , forcage_serre_CO2 = forcage_serre_CO2
        , forcage_serre_eau = forcage_serre_eau
        , g = g
        , phieq = calcul_phieq sv previousState.zT
        , tau_niveau_calottes = tau_niveau_calottes
        , zB_ocean = zB_ocean
        , zC_alteration = zC_alteration
        , zC_stockage = zC_stockage
        , zCO2 =
            if oscillation == 1 && not sv.fixed_concentration then
                (zCO2 + previousState.zCO2 + 0.5 * previousState.zCO2_prec) / 2.5

            else
                zCO2
        , zCO2_prec = previousState.zCO2
        , zCO2eq_oce = calcul_zCO2eq_oce previousState.zT
        , zT =
            if oscillation == 1 then
                (zT + previousState.zT + 0.5 * previousState.zT_ancien) / 2.5

            else
                zT
        , zT_ancien = previousState.zT
        , zTeq = zTeq
        , zalbedo = zalbedo
        , zphig =
            if oscillation == 1 then
                (zphig + previousState.zphig + 0.5 * previousState.zphig_ancien) / 2.5

            else
                zphig
        , zphig_ancien = previousState.zphig
        , zpuit_oce = zpuit_oce
        , zrapport_H2O = zrapport_H2O
        , zsomme_C = zsomme_C
        , zsomme_flux_const = zsomme_flux_const
    }


calcul_oscillation : SimulationValues -> Int -> State -> Float -> Float -> Int
calcul_oscillation sv iter previous zT zCO2 =
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
    if sv.fixed_concentration then
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


calcul_zCO2 : State -> Float -> Float -> Float
calcul_zCO2 previousState emission_coo_ppm dt_ =
    max 0 <|
        previousState.zCO2
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


calcul_fdegaz : SimulationValues -> Float -> Float
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


calcul_zC_stockage : SimulationValues -> Float -> Float
calcul_zC_stockage sv zphig_ancien =
    calcul_zC_alteration
        (-sv.stockage_biologique_value * 1.0e-3)
        zphig_ancien


calcul_fin : SimulationValues -> Float -> Float
calcul_fin sv zalbedo =
    calcul_fin0 sv * (1 - zalbedo)


calcul_albedo : SimulationValues -> Float -> Float
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
    if sv.fixed_concentration then
        -- undefined in SimClimat
        0

    else if sv.debranche_biologie then
        0

    else
        sv.puit_bio_value / 100


calcul_zpuit_oce : SimulationValues -> Float -> Float
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


calcul_zB_ocean : State -> Float
calcul_zB_ocean previousState =
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
calcul_zphig sv previousState tau_niveau_calottes =
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
    PhysicsConstants.c_alteration_naturel * (sv.alteration_value / 100.0)


calcul_b_ocean : SimulationValues -> Float
calcul_b_ocean sv =
    if sv.debranche_ocean then
        0

    else if sv.fixed_ocean then
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
                            - (0.3 * sv.excentricite_value + 0.7 * PhysicsConstants.excentricite_actuel)
                            * 0.5
                            * sin (-sv.precession_value / 180 * pi)
                          )
                    )
            )


delta_angle : SimulationValues -> Float
delta_angle sv =
    abs
        ((toFloat PhysicsConstants.lat_Mil - sv.obliquite_value)
            / 360
            * 2
            * PhysicsConstants.pi
        )


calcul_fin0 : SimulationValues -> Float
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



--  Math.max(4, Math.trunc(3 * Math.exp(0.3 * Math.log(this.experienceValues.temps_elem()))));


temperature_past_data : InitialState -> List Float
temperature_past_data initialState =
    (::) 0 <|
        case initialState of
            PreIndustrial ->
                List.repeat n 14.399999999999977

            Now ->
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


simulate : SimulationValues -> SimulationValues
simulate config =
    { initialState = config.initialState
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
    , results = boucleT config
    }
