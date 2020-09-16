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
    { -- CONFIG
      fixed_eau : Bool
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


temperature_data : DataArray Float
temperature_data =
    { datas = List.repeat (n + 1) 14.399999999999977
    , past_datas = 0 :: List.repeat n 14.399999999999977
    , resolution = 100
    , indice_min = 0
    , indice_max = 100
    , imin = 0
    , imax = 100
    }


simulate : Config -> Simulation
simulate config =
    { fixed_eau = config.fixed_eau
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
    , temperature_data = temperature_data
    }
