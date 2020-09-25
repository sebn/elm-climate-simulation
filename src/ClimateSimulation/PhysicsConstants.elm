module ClimateSimulation.PhysicsConstants exposing
    ( a_H2O
    , a_calottes
    , a_coo
    , a_rankine
    , albedo_1750
    , albedo_crit
    , albedo_glace_const
    , albedo_ter
    , b_calottes
    , b_ocean
    , b_rankine
    , c_alteration_naturel
    , c_calottes
    , concentration_coo_1750
    , concentration_coo_actuel
    , concentration_coo_glaciaire
    , concentration_coo_limite
    , concentration_coo_limite_bas
    , coo_Gt_act
    , dFdegaz
    , deltaT_last_century
    , excentricite_actuel
    , g0
    , g_min
    , insol_actuel
    , lat_Mil
    , niveau_calotte_critique_coo
    , niveau_calottes_1750
    , niveau_calottes_LGM_noinsol
    , niveau_calottes_actuel
    , niveau_calottes_max
    , niveau_calottes_min
    , obliquite_actuel
    , phig_crit
    , pi
    , pow_H2O
    , precession_actuel
    , puissance_recue_zero
    , puit_oce_max
    , puit_ocean_act
    , q_CO2
    , q_H2O
    , sigma
    , tKelvin
    , tau_niveau_calottes_deglacement
    , tau_niveau_calottes_englacement
    , tau_temperature
    , tcrit_oce
    , temperature_1750
    , temperature_LGM
    , temperature_actuelle
    , volcanisme_actuel
    )

import ClimateSimulation.Math exposing (exp, log)


a_H2O : Float
a_H2O =
    -q_H2O * (1 - g0)


{-| On suppose qu'une partie de la variation du niveau de calotte est lié à
l'effet de la température (ici 40%), et l'autre est liée à l'effet de la
variation d'insolation.
-}
a_calottes : Float
a_calottes =
    (niveau_calottes_1750 - niveau_calottes_LGM_noinsol)
        / (temperature_1750 - temperature_LGM)
        * 0.8


a_coo : Float
a_coo =
    1.8e-2


a_rankine : Float
a_rankine =
    13.7


albedo_1750 : Float
albedo_1750 =
    0.33


albedo_crit : Float
albedo_crit =
    (albedo_1750
        - albedo_ter
        * (niveau_calottes_1750 - phig_crit)
        / (niveau_calottes_max - phig_crit)
    )
        / (1
            - (niveau_calottes_1750 - phig_crit)
            / (niveau_calottes_max - phig_crit)
          )


albedo_glace_const : Float
albedo_glace_const =
    0.9


albedo_ter : Float
albedo_ter =
    0.25


b_calottes : Float
b_calottes =
    niveau_calottes_1750 - (a_calottes * temperature_1750)


b_ocean : Float
b_ocean =
    1 / 5000.0


b_rankine : Float
b_rankine =
    5120


c_alteration_naturel : Float
c_alteration_naturel =
    -volcanisme_actuel / concentration_coo_1750


c_calottes : Float
c_calottes =
    0.2


concentration_coo_1750 : Float
concentration_coo_1750 =
    280


delta_angle_actuel : Float
delta_angle_actuel =
    (toFloat lat_Mil - obliquite_actuel)
        / 360
        * 2
        * pi


excentricite_actuel : Float
excentricite_actuel =
    0.0167


g0 : Float
g0 =
    puissance_recue_zero
        * (1 - albedo_1750)
        / sigma
        / exp (4 * log (tKelvin + temperature_1750))


{-| `forcage_serre` min, pour éviter des NaN
-}
g_min : Float
g_min =
    1.0e-4


insol_actuel : Float
insol_actuel =
    puissance_recue_zero * cos delta_angle_actuel


lat_Mil : Int
lat_Mil =
    65


niveau_calotte_critique_coo : Float
niveau_calotte_critique_coo =
    20


niveau_calottes_1750 : Float
niveau_calottes_1750 =
    60


niveau_calottes_LGM_noinsol : Float
niveau_calottes_LGM_noinsol =
    52


niveau_calottes_actuel : Float
niveau_calottes_actuel =
    niveau_calottes_1750
        + (a_calottes
            * temperature_actuelle
            + b_calottes
            - niveau_calottes_1750
          )
        * (1 - exp (-100 / tau_niveau_calottes_deglacement))


niveau_calottes_max : Float
niveau_calottes_max =
    90


niveau_calottes_min : Float
niveau_calottes_min =
    0


obliquite_actuel : Float
obliquite_actuel =
    23.5


phig_crit : Float
phig_crit =
    30


pi : Float
pi =
    Basics.pi


{-| normallement: 0.18
-}
pow_H2O : Float
pow_H2O =
    0.23


precession_actuel : Float
precession_actuel =
    -- on prend la même precession que LMDZ
    -- la formule de insol65N n'a pas été modifiée car 2 bugs se compensaient
    102.7


puissance_recue_zero : Float
puissance_recue_zero =
    1370 / 4


{-| en années
-}
tau_temperature : Float
tau_temperature =
    30


tcrit_oce : Float
tcrit_oce =
    35


temperature_1750 : Float
temperature_1750 =
    14.4


temperature_LGM : Float
temperature_LGM =
    10


temperature_actuelle : Float
temperature_actuelle =
    15.3


{-| ppm
-}
concentration_coo_actuel : Float
concentration_coo_actuel =
    405


{-| ppm
-}
concentration_coo_glaciaire : Float
concentration_coo_glaciaire =
    180


{-| en ppm, la limite entre relation linéaire et log pour forçage serre
-}
concentration_coo_limite : Float
concentration_coo_limite =
    10000


{-| en ppm, idem pour extrapolation pour basses concentrations
-}
concentration_coo_limite_bas : Float
concentration_coo_limite_bas =
    100


{-| masse de CO2 en Gt
-}
coo_Gt_act : Float
coo_Gt_act =
    750


dFdegaz : Float
dFdegaz =
    0.05


deltaT_last_century : Float
deltaT_last_century =
    0.8


{-| en pourcent
-}
puit_oce_max : Float
puit_oce_max =
    40


{-| en pourcent des emissions anthro absorbées
-}
puit_ocean_act : Float
puit_ocean_act =
    20


{-| CO2 = 26% de l'effet de serre
-}
q_CO2 : Float
q_CO2 =
    0.26


{-| eau = 60% de l'effet de serre
-}
q_H2O : Float
q_H2O =
    0.6


sigma : Float
sigma =
    5.67e-8


tKelvin : Float
tKelvin =
    273.0


tau_niveau_calottes_deglacement : Float
tau_niveau_calottes_deglacement =
    -- FIXME: same as tau_niveau_calottes_englacement? why?
    4000


tau_niveau_calottes_englacement : Float
tau_niveau_calottes_englacement =
    -- FIXME: same as tau_niveau_calottes_deglacement? why?
    4000


{-| en Gt/an
-}
volcanisme_actuel : Float
volcanisme_actuel =
    0.083
