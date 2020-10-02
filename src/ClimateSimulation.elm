module ClimateSimulation exposing
    ( ClimateSimulation
    , fromSimClimat
    , run
    , toSimClimat
    )

import ClimateSimulation.Duration as Duration
import ClimateSimulation.Parameters as Parameters exposing (Parameters)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import ClimateSimulation.State as State exposing (State)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import List.Nonempty as NEList


type alias ClimateSimulation =
    { name : String
    , parameters : Parameters
    , results : List State
    }


run : ClimateSimulation -> ClimateSimulation
run simulation =
    let
        initialState =
            State.initial simulation.parameters
    in
    { simulation
        | results =
            List.range 1 n
                |> List.foldl
                    (prependNextState simulation.parameters)
                    (NEList.fromElement initialState)
                |> NEList.reverse
                |> NEList.tail
                |> (::) initialState
    }


n : Int
n =
    100


prependNextState : Parameters -> Int -> NEList.Nonempty State -> NEList.Nonempty State
prependNextState parameters t previousStates =
    let
        previousState =
            NEList.head previousStates

        nextState =
            State.next parameters t previousState
    in
    NEList.cons nextState previousStates



-- RESULTS


albedo_data : ClimateSimulation -> List Float
albedo_data sv =
    List.map State.albedoPercentage sv.results


emissions_coo_data : ClimateSimulation -> List Float
emissions_coo_data sv =
    List.map2 (calcul_emission_coo sv)
        sv.results
        (sv.results |> List.tail |> Maybe.withDefault [])


calcul_emission_coo : ClimateSimulation -> State -> State -> Float
calcul_emission_coo sv previousState state =
    if sv.parameters.fixed_concentration then
        0

    else
        (State.co2Concentration state
            - State.co2Concentration previousState
        )
            / Duration.temps_elem sv.parameters.duration


niveau_mer_data : ClimateSimulation -> List Float
niveau_mer_data sv =
    List.indexedMap (calcul_niveau_mer sv) sv.results


calcul_niveau_mer : ClimateSimulation -> Int -> State -> Float
calcul_niveau_mer sv t state =
    if t == 0 then
        Parameters.niveau_mer0 sv.parameters

    else
        let
            zT =
                State.temperature state

            zphig =
                State.iceCap state

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



-- SIMCLIMAT


fromSimClimat : JD.Value -> Result JD.Error ClimateSimulation
fromSimClimat json =
    JD.decodeValue simulationValuesDecoder json


simulationValuesDecoder : JD.Decoder ClimateSimulation
simulationValuesDecoder =
    JD.succeed ClimateSimulation
        |> JDP.required "simulation_name" JD.string
        |> JDP.custom Parameters.simClimatDecoder
        |> JDP.hardcoded []


toSimClimat : ClimateSimulation -> JE.Value
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


duplicateLast : List a -> List a
duplicateLast items =
    case items of
        [] ->
            items

        [ x ] ->
            [ x, x ]

        first :: rest ->
            first :: duplicateLast rest


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
