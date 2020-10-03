module ClimateSimulation.Parameters exposing
    ( Parameters
    , alteration_max
    , b_ocean
    , fin0
    , insol65N
    , niveau_mer0
    , simClimatDecoder
    , temperature_past_data
    , toSimClimatFields
    , zT0
    , zphig0
    , zpuit_bio
    , zpuit_oce0
    )

import ClimateSimulation.Duration as Duration exposing (Duration)
import ClimateSimulation.Math exposing (exp, log)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE



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


type alias Parameters =
    { initialState : InitialState
    , duration : Duration
    , fixed_eau : Bool
    , fixed_concentration : Bool
    , debranche_biologie : Bool
    , fixed_ocean : Bool
    , debranche_ocean : Bool
    , fixed_albedo : Bool
    , rapport_H2O_value : Float
    , puit_bio_value : Float

    -- FIXME: Does puit_oce_value actually change anything?
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


n : Int
n =
    100


tempsElem : Float
tempsElem =
    1.0


internEcheance : Float
internEcheance =
    100.0


niveau_mer0 : Parameters -> Float
niveau_mer0 params =
    case params.initialState of
        PreIndustrial ->
            PhysicsConstants.niveau_mer_1750

        _ ->
            0


temperature_past_data : Parameters -> List Float
temperature_past_data parameters =
    (::) 0 <|
        case parameters.initialState of
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


zT0 : Parameters -> Float
zT0 params =
    (+) PhysicsConstants.tKelvin <|
        case params.initialState of
            PreIndustrial ->
                PhysicsConstants.temperature_1750

            _ ->
                PhysicsConstants.temperature_actuelle


zphig0 : Parameters -> Float
zphig0 params =
    case params.initialState of
        PreIndustrial ->
            PhysicsConstants.niveau_calottes_1750

        _ ->
            PhysicsConstants.niveau_calottes_actuel


zpuit_oce0 : Parameters -> Float
zpuit_oce0 parameters =
    if parameters.fixed_concentration || parameters.debranche_ocean then
        0

    else
        parameters.puit_oce_value / 100.0


endYear : Parameters -> Int
endYear { initialState, duration } =
    startYear initialState + Duration.intoYears duration


alteration_max : Parameters -> Float
alteration_max parameters =
    PhysicsConstants.c_alteration_naturel * (parameters.alteration_value / 100.0)


b_ocean : Parameters -> Float
b_ocean parameters =
    if parameters.debranche_ocean then
        0

    else if parameters.fixed_ocean then
        0

    else
        PhysicsConstants.b_ocean


{-| Insolation 65º lat. N
-}
insol65N : Parameters -> Float
insol65N parameters =
    fin0 parameters
        * cos (delta_angle parameters)
        * exp
            (2
                * log
                    ((1
                        - PhysicsConstants.excentricite_actuel
                        * 0.5
                        * sin (-PhysicsConstants.precession_actuel / 180 * pi)
                     )
                        / (1
                            - (0.3 * parameters.excentricite_value + 0.7 * PhysicsConstants.excentricite_actuel)
                            * 0.5
                            * sin (-parameters.precession_value / 180 * pi)
                          )
                    )
            )


delta_angle : Parameters -> Float
delta_angle parameters =
    abs
        ((toFloat PhysicsConstants.lat_Mil - parameters.obliquite_value)
            / 360
            * 2
            * PhysicsConstants.pi
        )


fin0 : Parameters -> Float
fin0 parameters =
    PhysicsConstants.puissance_recue_zero
        * (parameters.puissance_soleil_value / 100.0)
        / (parameters.distance_ts_value / 100)
        / (parameters.distance_ts_value / 100)


zpuit_bio : Parameters -> Float
zpuit_bio parameters =
    if parameters.fixed_concentration then
        0

    else if parameters.debranche_biologie then
        0

    else
        parameters.puit_bio_value / 100


simClimatDecoder : JD.Decoder Parameters
simClimatDecoder =
    JD.succeed Parameters
        |> JDP.required "annee_debut" initialStateDecoder
        |> JDP.custom durationDecoder
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


durationDecoder : JD.Decoder Duration
durationDecoder =
    JD.oneOf
        [ yearRangeDecoder |> JD.andThen durationDecoderFromYearRange
        , JD.succeed (Duration.fromYears 10000)
        ]


yearRangeDecoder : JD.Decoder ( Int, Int )
yearRangeDecoder =
    JD.map2 Tuple.pair
        (JD.field "annee_debut" JD.int)
        (JD.field "annee_fin" JD.int)


durationDecoderFromYearRange : ( Int, Int ) -> JD.Decoder Duration
durationDecoderFromYearRange ( annee_debut, annee_fin ) =
    (annee_fin - annee_debut)
        |> Duration.fromYears
        |> JD.succeed


toSimClimatFields : Parameters -> List ( String, JE.Value )
toSimClimatFields params =
    [ ( "annee_debut", JE.int (startYear params.initialState) )
    , ( "annee_fin", JE.int (endYear params) )
    , ( "fixed_eau", JE.bool params.fixed_eau )
    , ( "fixed_concentration", JE.bool params.fixed_concentration )
    , ( "debranche_biologie", JE.bool params.debranche_biologie )
    , ( "fixed_ocean", JE.bool params.fixed_ocean )
    , ( "debranche_ocean", JE.bool params.debranche_ocean )
    , ( "fixed_albedo", JE.bool params.fixed_albedo )
    , ( "rapport_H2O_value", JE.float params.rapport_H2O_value )
    , ( "puit_bio_value", JE.float params.puit_bio_value )
    , ( "puit_oce_value", JE.float params.puit_oce_value )
    , ( "albedo_value", JE.float params.albedo_value )
    , ( "coo_concentr_value", JE.float params.coo_concentr_value )
    , ( "puissance_soleil_value", JE.float params.puissance_soleil_value )
    , ( "distance_ts_value", JE.float params.distance_ts_value )
    , ( "obliquite_value", JE.float params.obliquite_value )
    , ( "excentricite_value", JE.float params.excentricite_value )
    , ( "precession_value", JE.float params.precession_value )
    , ( "alteration_value", JE.float params.alteration_value )
    , ( "emit_anthro_coo_value", JE.float params.emit_anthro_coo_value )
    , ( "volcan_value", JE.float params.volcan_value )
    , ( "stockage_biologique_value", JE.float params.stockage_biologique_value )
    ]
