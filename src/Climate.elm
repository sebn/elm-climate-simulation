module Climate exposing
    ( Config
    , Simulation
    , simulate
    )


type alias Config =
    { fixed_eau : Bool
    , fixed_concentration : Bool
    , debranche_biologie : Bool
    , fixed_ocean : Bool
    , debranche_ocean : Bool
    , fixed_albedo : Bool
    , rapport_H2O_value : Int
    , puit_bio_value : Int
    , puit_oce_value : Int
    , albedo_value : Int
    , coo_concentr_value : Int
    , puissance_soleil_value : Int
    , distance_ts_value : Int
    , obliquite_value : Float
    , excentricite_value : Float
    , precession_value : Float
    , alteration_value : Int
    , emit_anthro_coo_value : Int
    , volcan_value : Float
    , stockage_biologique_value : Int
    }


type alias Simulation =
    Config


simulate : Config -> Simulation
simulate =
    identity
