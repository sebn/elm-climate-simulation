module Ui.Data.ParametersForm exposing
    ( AnthropogenicEmissions(..)
    , BiologicalStorage(..)
    , Co2(..)
    , Co2Concentration(..)
    , ContinentalAlteration(..)
    , EarthSunDistance(..)
    , Excentricity(..)
    , Obliquity(..)
    , OceanicCarbonSink(..)
    , ParametersForm
    , PlanetaryAlbedo(..)
    , Precession(..)
    , SolarPower(..)
    , VegetationCarbonSink(..)
    , VolcanicEmissions(..)
    , WaterVaporConcentration(..)
    , fromParameters
    , toParameters
    )

import ClimateSimulation.Duration as Duration
import ClimateSimulation.Parameters as Parameters exposing (Parameters)


type alias ParametersForm =
    { initialState : Parameters.InitialState
    , simulationLength : String
    , earthSunDistance : EarthSunDistance
    , earthSunDistanceCustomValue : String
    , solarPower : SolarPower
    , solarPowerCustomValue : String
    , excentricity : Excentricity
    , excentricityCustomValue : String
    , obliquity : Obliquity
    , obliquityCustomValue : String
    , precession : Precession
    , precessionCustomValue : String
    , co2 : Co2
    , co2Concentration : Co2Concentration
    , co2ConcentrationCustomValue : String
    , anthropogenicEmissions : AnthropogenicEmissions
    , anthropogenicEmissionsCustomValue : String
    , volcanicEmissions : VolcanicEmissions
    , volcanicEmissionsCustomValue : String
    , biologicalStorage : BiologicalStorage
    , biologicalStorageCustomValue : String
    , continentalAlteration : ContinentalAlteration
    , continentalAlterationCustomValue : String
    , planetaryAlbedo : PlanetaryAlbedo
    , planetaryAlbedoCustomValue : String
    , oceanicCarbonSink : OceanicCarbonSink
    , oceanicCarbonSinkCustomValue : String
    , vegetationCarbonSink : VegetationCarbonSink
    , vegetationCarbonSinkCustomValue : String
    , waterVaporConcentration : WaterVaporConcentration
    , waterVaporConcentrationCustomValue : String
    }


type EarthSunDistance
    = EarthSunDistancePresentDay
    | EarthSunDistanceCustom


type SolarPower
    = SolarPowerPresentDay
    | SolarPowerEarthBeginning
    | SolarPowerCustom


type Excentricity
    = ExcentricityPresentDay
    | ExcentricityMinimum
    | ExcentricityMaximum
    | ExcentricityCustom


type Obliquity
    = ObliquityPresentDay
    | ObliquityMinimum
    | ObliquityMaximum
    | ObliquityCustom


type Precession
    = PrecessionPresentDay
    | PrecessionMinimum
    | PrecessionMaximum
    | PrecessionCustom


type Co2
    = Co2Constant
    | Co2SourcesAndSinks


type Co2Concentration
    = Co2ConcentrationPresentDay
    | Co2ConcentrationPreIndustrial
    | Co2ConcentrationCretaceous
    | Co2ConcentrationEarthBeginning
    | Co2ConcentrationCustom


type AnthropogenicEmissions
    = AnthropogenicEmissionsNull
    | AnthropogenicEmissionsPresentDay
    | AnthropogenicEmissionsTwicePresentDay
    | AnthropogenicEmissionsCustom


type VolcanicEmissions
    = VolcanicEmissionsPresentDay
    | VolcanicEmissionsEarthBeginning
    | VolcanicEmissionsCustom


type BiologicalStorage
    = BiologicalStoragePresentDay
    | BiologicalStorageCarboniferous
    | BiologicalStorageCustom


type ContinentalAlteration
    = ContinentalAlterationPresentDay
    | ContinentalAlterationCustom


type PlanetaryAlbedo
    = PlanetaryAlbedoComputed
    | PlanetaryAlbedoPresentDay
    | PlanetaryAlbedoPreIndustrial
    | PlanetaryAlbedoSameAsSoil
    | PlanetaryAlbedoSameAsIce
    | PlanetaryAlbedoCustom


type OceanicCarbonSink
    = OceanicCarbonSinkNeglected
    | OceanicCarbonSinkComputed
    | OceanicCarbonSinkConstantPresentDay
    | OceanicCarbonSinkConstantCustom


type VegetationCarbonSink
    = VegetationCarbonSinkNeglected
    | VegetationCarbonSinkAsToday
    | VegetationCarbonSinkCustom


type WaterVaporConcentration
    = WaterVaporConcentrationComputed
    | WaterVaporConcentrationConstantPresentDay
    | WaterVaporConcentrationConstantPreIndustrial
    | WaterVaporConcentrationConstantCustom


fromParameters : Parameters -> ParametersForm
fromParameters parameters =
    { initialState = parameters.initialState
    , simulationLength = parameters.duration |> Duration.intoYears |> String.fromInt
    , earthSunDistance =
        if parameters.distance_ts_value == 100 then
            EarthSunDistancePresentDay

        else
            EarthSunDistanceCustom
    , earthSunDistanceCustomValue = parameters.distance_ts_value |> String.fromFloat
    , solarPower =
        if parameters.puissance_soleil_value == 100 then
            SolarPowerPresentDay

        else if parameters.puissance_soleil_value == 70 then
            SolarPowerEarthBeginning

        else
            SolarPowerCustom
    , solarPowerCustomValue = parameters.puissance_soleil_value |> String.fromFloat
    , excentricity =
        if parameters.excentricite_value == 0.0167 then
            ExcentricityPresentDay

        else if parameters.excentricite_value == 0 then
            ExcentricityMinimum

        else if parameters.excentricite_value == 0.06 then
            ExcentricityMaximum

        else
            ExcentricityCustom
    , excentricityCustomValue = parameters.excentricite_value |> String.fromFloat
    , obliquity =
        if parameters.obliquite_value == 23.5 then
            ObliquityPresentDay

        else if parameters.obliquite_value == 21.8 then
            ObliquityMinimum

        else if parameters.obliquite_value == 24.4 then
            ObliquityMaximum

        else
            ObliquityCustom
    , obliquityCustomValue = parameters.obliquite_value |> String.fromFloat
    , precession =
        if parameters.precession_value == 102.7 then
            PrecessionPresentDay

        else if parameters.precession_value == 90 then
            PrecessionMinimum

        else if parameters.precession_value == 270 then
            PrecessionMaximum

        else
            PrecessionCustom
    , precessionCustomValue = parameters.precession_value |> String.fromFloat
    , co2 =
        if parameters.fixed_concentration then
            Co2Constant

        else
            Co2SourcesAndSinks
    , co2Concentration =
        if parameters.coo_concentr_value == 405 then
            Co2ConcentrationPresentDay

        else if parameters.coo_concentr_value == 280 then
            Co2ConcentrationPreIndustrial

        else if parameters.coo_concentr_value == 1500 then
            Co2ConcentrationCretaceous

        else if parameters.coo_concentr_value == 300000 then
            Co2ConcentrationEarthBeginning

        else
            Co2ConcentrationCustom
    , co2ConcentrationCustomValue = parameters.coo_concentr_value |> String.fromFloat
    , anthropogenicEmissions =
        if parameters.emit_anthro_coo_value == 0 then
            AnthropogenicEmissionsNull

        else if parameters.emit_anthro_coo_value == 8 then
            AnthropogenicEmissionsPresentDay

        else if parameters.emit_anthro_coo_value == 16 then
            AnthropogenicEmissionsTwicePresentDay

        else
            AnthropogenicEmissionsCustom
    , anthropogenicEmissionsCustomValue = parameters.emit_anthro_coo_value |> String.fromFloat
    , volcanicEmissions =
        if parameters.volcan_value == 0.083 then
            VolcanicEmissionsPresentDay

        else if parameters.volcan_value == 0.42 then
            VolcanicEmissionsEarthBeginning

        else
            VolcanicEmissionsCustom
    , volcanicEmissionsCustomValue = parameters.volcan_value |> String.fromFloat
    , biologicalStorage =
        if parameters.stockage_biologique_value == 0 then
            BiologicalStoragePresentDay

        else if parameters.stockage_biologique_value == 0.71 then
            BiologicalStorageCarboniferous

        else
            BiologicalStorageCustom
    , biologicalStorageCustomValue = parameters.stockage_biologique_value |> String.fromFloat
    , continentalAlteration =
        if parameters.alteration_value == 100 then
            ContinentalAlterationPresentDay

        else
            ContinentalAlterationCustom
    , continentalAlterationCustomValue = parameters.alteration_value |> String.fromFloat
    , planetaryAlbedo =
        if not parameters.fixed_albedo then
            PlanetaryAlbedoComputed

        else if parameters.albedo_value == 33 then
            PlanetaryAlbedoPresentDay

        else if parameters.albedo_value == 33 then
            PlanetaryAlbedoPreIndustrial

        else if parameters.albedo_value == 25 then
            PlanetaryAlbedoSameAsSoil

        else if parameters.albedo_value == 90 then
            PlanetaryAlbedoSameAsIce

        else
            PlanetaryAlbedoCustom
    , planetaryAlbedoCustomValue = parameters.albedo_value |> String.fromFloat
    , oceanicCarbonSink =
        Debug.log "initial oceanicCarbonSink" <|
            case
                ( Debug.log "initial debranche_ocean" parameters.debranche_ocean
                , Debug.log "initial fixed_ocean" parameters.fixed_ocean
                , truncate <| Debug.log "initial puit_oce_value" parameters.puit_oce_value
                )
            of
                ( True, _, _ ) ->
                    OceanicCarbonSinkNeglected

                ( False, True, 20 ) ->
                    OceanicCarbonSinkConstantPresentDay

                ( False, True, _ ) ->
                    OceanicCarbonSinkConstantCustom

                ( False, False, _ ) ->
                    OceanicCarbonSinkComputed
    , oceanicCarbonSinkCustomValue = parameters.puit_oce_value |> String.fromFloat
    , vegetationCarbonSink =
        if parameters.debranche_biologie then
            VegetationCarbonSinkNeglected

        else if parameters.puit_bio_value == 105.7 then
            VegetationCarbonSinkAsToday

        else
            VegetationCarbonSinkCustom
    , vegetationCarbonSinkCustomValue = parameters.puit_bio_value |> String.fromFloat
    , waterVaporConcentration =
        if not parameters.fixed_eau then
            WaterVaporConcentrationComputed

        else if parameters.rapport_H2O_value == 105.7 then
            WaterVaporConcentrationConstantPresentDay

        else if parameters.rapport_H2O_value == 100 then
            WaterVaporConcentrationConstantPreIndustrial

        else
            WaterVaporConcentrationConstantCustom
    , waterVaporConcentrationCustomValue = parameters.rapport_H2O_value |> String.fromFloat
    }


toParameters : ParametersForm -> Parameters -> Parameters
toParameters parametersForm defaults =
    { initialState = parametersForm.initialState
    , duration =
        parametersForm.simulationLength
            |> String.toInt
            |> Maybe.andThen (Just << Duration.fromYears)
            |> Maybe.withDefault defaults.duration
    , fixed_eau =
        parametersForm.waterVaporConcentration /= WaterVaporConcentrationComputed
    , fixed_concentration =
        case parametersForm.co2 of
            Co2Constant ->
                True

            Co2SourcesAndSinks ->
                False
    , debranche_biologie =
        parametersForm.vegetationCarbonSink == VegetationCarbonSinkNeglected
    , fixed_ocean =
        Debug.log "fixed_ocean" <|
            case Debug.log "oceanicCarbonSink" parametersForm.oceanicCarbonSink of
                OceanicCarbonSinkNeglected ->
                    False

                OceanicCarbonSinkComputed ->
                    False

                _ ->
                    True
    , debranche_ocean =
        Debug.log "debranche_ocean" <|
            case parametersForm.oceanicCarbonSink of
                OceanicCarbonSinkNeglected ->
                    True

                _ ->
                    False
    , fixed_albedo =
        case parametersForm.planetaryAlbedo of
            PlanetaryAlbedoComputed ->
                False

            _ ->
                True
    , rapport_H2O_value =
        parametersForm.waterVaporConcentrationCustomValue
            |> String.toFloat
            |> Maybe.withDefault defaults.rapport_H2O_value
    , puit_bio_value =
        parametersForm.vegetationCarbonSinkCustomValue
            |> String.toFloat
            |> Maybe.withDefault defaults.puit_bio_value
    , puit_oce_value =
        Debug.log "puit_oce_value" <|
            case parametersForm.oceanicCarbonSink of
                OceanicCarbonSinkConstantPresentDay ->
                    20

                _ ->
                    parametersForm.oceanicCarbonSinkCustomValue
                        |> String.toFloat
                        |> Maybe.withDefault defaults.puit_oce_value
    , albedo_value =
        Maybe.withDefault defaults.albedo_value <|
            case parametersForm.planetaryAlbedo of
                PlanetaryAlbedoComputed ->
                    Nothing

                PlanetaryAlbedoPresentDay ->
                    Just 33

                PlanetaryAlbedoPreIndustrial ->
                    Just 33

                PlanetaryAlbedoSameAsSoil ->
                    Just 25

                PlanetaryAlbedoSameAsIce ->
                    Just 90

                PlanetaryAlbedoCustom ->
                    String.toFloat parametersForm.planetaryAlbedoCustomValue
    , coo_concentr_value =
        case parametersForm.co2Concentration of
            Co2ConcentrationPresentDay ->
                405

            Co2ConcentrationPreIndustrial ->
                280

            Co2ConcentrationCretaceous ->
                1500

            Co2ConcentrationEarthBeginning ->
                300000

            Co2ConcentrationCustom ->
                parametersForm.co2ConcentrationCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.coo_concentr_value
    , puissance_soleil_value =
        case parametersForm.solarPower of
            SolarPowerPresentDay ->
                100

            SolarPowerEarthBeginning ->
                70

            SolarPowerCustom ->
                parametersForm.solarPowerCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.puissance_soleil_value
    , distance_ts_value =
        case parametersForm.earthSunDistance of
            EarthSunDistancePresentDay ->
                100

            EarthSunDistanceCustom ->
                parametersForm.earthSunDistanceCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.distance_ts_value
    , obliquite_value =
        case parametersForm.obliquity of
            ObliquityPresentDay ->
                23.5

            ObliquityMinimum ->
                21.8

            ObliquityMaximum ->
                24.4

            ObliquityCustom ->
                parametersForm.obliquityCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.obliquite_value
    , excentricite_value =
        case parametersForm.excentricity of
            ExcentricityPresentDay ->
                0.0167

            ExcentricityMinimum ->
                0

            ExcentricityMaximum ->
                0.06

            ExcentricityCustom ->
                parametersForm.excentricityCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.excentricite_value
    , precession_value =
        case parametersForm.precession of
            PrecessionPresentDay ->
                102.7

            PrecessionMinimum ->
                90

            PrecessionMaximum ->
                270

            PrecessionCustom ->
                parametersForm.precessionCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.precession_value
    , alteration_value =
        case parametersForm.continentalAlteration of
            ContinentalAlterationPresentDay ->
                100

            ContinentalAlterationCustom ->
                parametersForm.continentalAlterationCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.alteration_value
    , emit_anthro_coo_value =
        case parametersForm.anthropogenicEmissions of
            AnthropogenicEmissionsNull ->
                0

            AnthropogenicEmissionsPresentDay ->
                8

            AnthropogenicEmissionsTwicePresentDay ->
                16

            AnthropogenicEmissionsCustom ->
                parametersForm.anthropogenicEmissionsCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.emit_anthro_coo_value
    , volcan_value =
        case parametersForm.volcanicEmissions of
            VolcanicEmissionsPresentDay ->
                0.083

            VolcanicEmissionsEarthBeginning ->
                0.42

            VolcanicEmissionsCustom ->
                parametersForm.volcanicEmissionsCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.volcan_value
    , stockage_biologique_value =
        case parametersForm.biologicalStorage of
            BiologicalStoragePresentDay ->
                0

            BiologicalStorageCarboniferous ->
                0.71

            BiologicalStorageCustom ->
                parametersForm.biologicalStorageCustomValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.stockage_biologique_value
    }
