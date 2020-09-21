module ModelPhysicsConstants exposing
    ( a_calottes
    , b_calottes
    , c_alteration_naturel
    , insol_actuel
    , niveau_calottes_actuel
    )

{-| "calculate calculated physics constants"
-}

import PhysicsConstants


{-| On suppose qu'une partie de la variation du niveau de calotte est lié à
l'effet de la température (ici 40%), et l'autre est liée à l'effet de la
variation d'insolation.
-}
a_calottes : Float
a_calottes =
    (PhysicsConstants.niveau_calottes_1750 - PhysicsConstants.niveau_calottes_LGM_noinsol)
        / (PhysicsConstants.temperature_1750 - PhysicsConstants.temperature_LGM)
        * 0.8


b_calottes : Float
b_calottes =
    PhysicsConstants.niveau_calottes_1750
        - (a_calottes * PhysicsConstants.temperature_1750)


c_alteration_naturel : Float
c_alteration_naturel =
    -PhysicsConstants.volcanisme_actuel
        / toFloat PhysicsConstants.concentration_coo_1750


delta_angle_actuel : Float
delta_angle_actuel =
    (toFloat PhysicsConstants.lat_Mil - PhysicsConstants.obliquite_actuel)
        / 360
        * 2
        * PhysicsConstants.pi


insol_actuel : Float
insol_actuel =
    PhysicsConstants.puissance_recue_zero * cos delta_angle_actuel


niveau_calottes_actuel : Float
niveau_calottes_actuel =
    PhysicsConstants.niveau_calottes_1750
        + (a_calottes
            * PhysicsConstants.temperature_actuelle
            + b_calottes
            - PhysicsConstants.niveau_calottes_1750
          )
        * (1
            - exp
                (-100
                    / PhysicsConstants.tau_niveau_calottes_deglacement
                )
          )


exp : Float -> Float
exp =
    (^) e
