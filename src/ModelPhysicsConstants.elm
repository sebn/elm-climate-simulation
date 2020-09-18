module ModelPhysicsConstants exposing (c_alteration_naturel)

{-| "calculate calculated physics constants"
-}

import PhysicsConstants


c_alteration_naturel : Float
c_alteration_naturel =
    -PhysicsConstants.volcanisme_actuel
        / toFloat PhysicsConstants.concentration_coo_1750
