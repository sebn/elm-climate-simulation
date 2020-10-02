module ClimateSimulation.State exposing
    (  -- FIXME: Make State opaque
       State(..)

    , albedoPercentage
    , co2Concentration
    , iceCap
    , temperature
    , toJson
    )

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
