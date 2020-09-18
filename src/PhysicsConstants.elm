module PhysicsConstants exposing
    ( b_ocean
    , c_calottes
    , concentration_coo_1750
    , deltaT_last_century
    , excentricite_actuel
    , lat_Mil
    , niveau_calottes_1750
    , niveau_calottes_LGM_noinsol
    , niveau_calottes_max
    , niveau_calottes_min
    , obliquite_actuel
    , pi
    , precession_actuel
    , puissance_recue_zero
    , tKelvin
    , temperature_1750
    , temperature_LGM
    , temperature_actuelle
    , volcanisme_actuel
    )


b_ocean : Float
b_ocean =
    1 / 5000.0


c_calottes : Float
c_calottes =
    0.2


concentration_coo_1750 : Int
concentration_coo_1750 =
    280


excentricite_actuel : Float
excentricite_actuel =
    0.0167


lat_Mil : Int
lat_Mil =
    65


niveau_calottes_1750 : Float
niveau_calottes_1750 =
    60


niveau_calottes_LGM_noinsol : Float
niveau_calottes_LGM_noinsol =
    52


niveau_calottes_max : Float
niveau_calottes_max =
    90


niveau_calottes_min : Float
niveau_calottes_min =
    0


obliquite_actuel : Float
obliquite_actuel =
    23.5


pi : Float
pi =
    Basics.pi


precession_actuel : Float
precession_actuel =
    -- on prend la même precession que LMDZ
    -- la formule de insol65N n'a pas été modifiée car 2 bugs se compensaient
    102.7


puissance_recue_zero : Float
puissance_recue_zero =
    1370.0 / 4.0


temperature_1750 : Float
temperature_1750 =
    14.4


temperature_LGM : Float
temperature_LGM =
    10


temperature_actuelle : Float
temperature_actuelle =
    15.3


deltaT_last_century : Float
deltaT_last_century =
    0.8


tKelvin : Float
tKelvin =
    273.0


{-| en Gt/an
-}
volcanisme_actuel : Float
volcanisme_actuel =
    0.083
