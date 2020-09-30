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
    , dilat
    , excentricite_actuel
    , fphig1
    , fphig2
    , fphig3
    , g0
    , g_min
    , hmer_tot
    , hmeract
    , insol_actuel
    , lat_Mil
    , niveau_calotte_critique_coo
    , niveau_calottes_1750
    , niveau_calottes_LGM_noinsol
    , niveau_calottes_actuel
    , niveau_calottes_max
    , niveau_calottes_min
    , niveau_mer_1750
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
    , tau_niveau_mer
    , tau_temperature
    , tcrit_oce
    , temperature_1750
    , temperature_LGM
    , temperature_actuelle
    , toSimClimat
    , tressentie_act
    , volcanisme_actuel
    )

import ClimateSimulation.Math exposing (exp, log)
import Json.Encode as JE


toSimClimat : JE.Value
toSimClimat =
    JE.object
        [ ( "t_res_coo_actuel", JE.float t_res_coo_actuel )
        , ( "t_res_coo_90", JE.float t_res_coo_90 )
        , ( "t_res_coo_critique", JE.float t_res_coo_critique )
        , ( "temperature_1900", JE.float temperature_1900 )
        , ( "G0", JE.float g0 )
        , ( "T_ressentie_actuelle", JE.float t_ressentie_actuelle )
        , ( "a_calottes", JE.float a_calottes )
        , ( "b_calottes", JE.float b_calottes )
        , ( "niveau_calottes_actuel", JE.float niveau_calottes_actuel )
        , ( "Tressentie_act", JE.float tressentie_act )
        , ( "dilatation_1750", JE.float dilatation_1750 )
        , ( "optim1", JE.float optim1 )
        , ( "optim2", JE.float optim2 )
        , ( "fphig1", JE.float fphig1 )
        , ( "Hmeract", JE.float hmeract )
        , ( "niveau_mer_1750", JE.float niveau_mer_1750 )
        , ( "C_alteration_naturel", JE.float c_alteration_naturel )
        , ( "deltaT_poce", JE.float deltaT_poce )
        , ( "a_H2O", JE.float a_H2O )
        , ( "Tlim_bio_froid", JE.float tlim_bio_froid )
        , ( "delta_angle_actuel", JE.float delta_angle_actuel )
        , ( "insol_actuel", JE.float insol_actuel )
        , ( "albedo_crit", JE.float albedo_crit )
        , ( "albedo_actuel", JE.float albedo_actuel )
        , ( "stockage_max", JE.float stockage_max )
        , ( "concentration_coo_carbonifere", JE.float concentration_coo_carbonifere )
        , ( "stockage_carbonifere", JE.float stockage_carbonifere )
        ]


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


a_oce : Float
a_oce =
    3.0e-2


a_rankine : Float
a_rankine =
    13.7


albedo_1750 : Float
albedo_1750 =
    0.33


albedo_actuel : Float
albedo_actuel =
    albedo_crit
        + (niveau_calottes_actuel - phig_crit)
        / (niveau_calottes_max - phig_crit)
        * (albedo_ter - albedo_crit)


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


{-| en °C
-}
dilat : Float
dilat =
    2.4e-4


dilatation_1750 : Float
dilatation_1750 =
    dilat * 0.5 * (temperature_1750 - tressentie_act)


excentricite_actuel : Float
excentricite_actuel =
    0.0167


{-| GT/an-1
-}
flux_co2_stockage_max : Float
flux_co2_stockage_max =
    -10


{-| en Gt par an d'apres R.A. Berner et D.E. Canfield (1989)
-}
flux_co2_stockage_carbonifere : Float
flux_co2_stockage_carbonifere =
    -0.396


fphig1 : Float
fphig1 =
    (niveau_mer_1750_target / hmer_tot - optim1) / optim2


fphig2 : Float
fphig2 =
    1.0e-5


fphig3 : Float
fphig3 =
    -1.99447e-7


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


{-| en m
-}
hmer_tot : Float
hmer_tot =
    3.8e3


hmeract : Float
hmeract =
    hmer_tot
        * (1
            - fphig1
            * (niveau_calottes_actuel - niveau_calottes_max)
            - fphig2
            * ((niveau_calottes_actuel - niveau_calottes_max) ^ 2)
            - fphig3
            * ((niveau_calottes_actuel - niveau_calottes_max) ^ 3)
          )


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


niveau_mer_1750_target : Float
niveau_mer_1750_target =
    -0.2


niveau_mer_1750 : Float
niveau_mer_1750 =
    hmer_tot
        * (1
            + (dilat * 0.5 * (temperature_1750 - tressentie_act))
          )
        * (1
            - (fphig1 * (niveau_calottes_1750 - niveau_calottes_max))
            - (fphig2 * ((niveau_calottes_1750 - niveau_calottes_max) ^ 2))
            - (fphig3 * ((niveau_calottes_1750 - niveau_calottes_max) ^ 3))
          )
        - hmeract


obliquite_actuel : Float
obliquite_actuel =
    23.5


optim1 : Float
optim1 =
    (1 + dilatation_1750)
        * (1
            - fphig2
            * ((niveau_calottes_1750 - niveau_calottes_max) ^ 2)
            - fphig3
            * ((niveau_calottes_1750 - niveau_calottes_max) ^ 3)
          )
        - (1
            - fphig2
            * ((niveau_calottes_actuel - niveau_calottes_max) ^ 2)
            - fphig3
            * ((niveau_calottes_actuel - niveau_calottes_max) ^ 3)
          )


optim2 : Float
optim2 =
    (niveau_calottes_actuel - niveau_calottes_max)
        - (1 + dilatation_1750)
        * (niveau_calottes_1750 - niveau_calottes_max)



-- Facteur de pondération de la température il y a [[deltat0]] ans


p0 : Float
p0 =
    1


p1 : Float
p1 =
    5


p2 : Float
p2 =
    3


p3 : Float
p3 =
    1


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


{-| `tau_niveau_mer` ne peut dépasser 100 ans car c'est la mémoire maximale
d'une simul pour la simul précédente.
-}
tau_niveau_mer : Float
tau_niveau_mer =
    100


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


{-| en °C ; necessaire dans modele\_reset
-}
temperature_1900 : Float
temperature_1900 =
    temperature_actuelle - deltaT_last_century


temperature_LGM : Float
temperature_LGM =
    10


temperature_actuelle : Float
temperature_actuelle =
    15.3


temperature_froid : Float
temperature_froid =
    262


{-| ppm
-}
concentration_coo_actuel : Float
concentration_coo_actuel =
    405


concentration_coo_carbonifere : Float
concentration_coo_carbonifere =
    concentration_coo_1750 * 2


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



-- Retard en temps en année


deltat0 : Float
deltat0 =
    0


deltat1 : Float
deltat1 =
    5


deltat2 : Float
deltat2 =
    20


deltat3 : Float
deltat3 =
    50


deltaT_last_century : Float
deltaT_last_century =
    0.8


deltaT_poce : Float
deltaT_poce =
    0.5 / a_oce


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


stockage_carbonifere : Float
stockage_carbonifere =
    flux_co2_stockage_carbonifere / concentration_coo_carbonifere


stockage_max : Float
stockage_max =
    flux_co2_stockage_max / concentration_coo_1750


tKelvin : Float
tKelvin =
    273.0


t_ressentie_actuelle : Float
t_ressentie_actuelle =
    (1 / (p0 + p1 + p2 + p3))
        * (p0
            * temperature_actuelle
            + p1
            * temperature_actuelle
            - deltaT_last_century
            / 100
            * deltat1
            + p2
            * temperature_actuelle
            - deltaT_last_century
            / 100
            * deltat2
            + p3
            * temperature_actuelle
            - deltaT_last_century
            / 100
            * deltat3
          )


tau_niveau_calottes_deglacement : Float
tau_niveau_calottes_deglacement =
    -- FIXME: same as tau_niveau_calottes_englacement? why?
    4000


tau_niveau_calottes_englacement : Float
tau_niveau_calottes_englacement =
    -- FIXME: same as tau_niveau_calottes_deglacement? why?
    4000


t_res_coo_90 : Float
t_res_coo_90 =
    t_res_coo_actuel * 0.9


t_res_coo_actuel : Float
t_res_coo_actuel =
    280 / 0.083 / (concentration_coo_actuel / coo_Gt_act)


t_res_coo_critique : Float
t_res_coo_critique =
    t_res_coo_90
        + ((90 - niveau_calotte_critique_coo)
            * -(t_res_coo_90 - t_res_coo_actuel)
            / (90 - niveau_calottes_1750)
          )


tlim_bio_froid : Float
tlim_bio_froid =
    temperature_froid


{-| `tressentie_act` doit correspondre au calcul dans `calcul_niveau_mer`
-}
tressentie_act : Float
tressentie_act =
    temperature_actuelle - 0.8 * deltaT_last_century


{-| en Gt/an
-}
volcanisme_actuel : Float
volcanisme_actuel =
    0.083
