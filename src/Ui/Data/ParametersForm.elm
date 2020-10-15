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
    | PlanetaryAlbedoConstant Float
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
    , earthSunDistance = EarthSunDistanceCustom
    , earthSunDistanceCustomValue = parameters.distance_ts_value |> String.fromFloat
    , solarPower = SolarPowerCustom
    , solarPowerCustomValue = parameters.puissance_soleil_value |> String.fromFloat
    , excentricity = ExcentricityCustom
    , excentricityCustomValue = parameters.excentricite_value |> String.fromFloat
    , obliquity = ObliquityCustom
    , obliquityCustomValue = parameters.obliquite_value |> String.fromFloat
    , precession = PrecessionCustom
    , precessionCustomValue = parameters.precession_value |> String.fromFloat
    , co2 = Co2SourcesAndSinks
    , co2Concentration = Co2ConcentrationCustom
    , co2ConcentrationCustomValue = parameters.coo_concentr_value |> String.fromFloat
    , anthropogenicEmissions = AnthropogenicEmissionsCustom
    , anthropogenicEmissionsCustomValue = parameters.emit_anthro_coo_value |> String.fromFloat
    , volcanicEmissions = VolcanicEmissionsCustom
    , volcanicEmissionsCustomValue = parameters.volcan_value |> String.fromFloat
    , biologicalStorage = BiologicalStorageCustom
    , biologicalStorageCustomValue = parameters.stockage_biologique_value |> String.fromFloat
    , continentalAlteration = ContinentalAlterationCustom
    , continentalAlterationCustomValue = parameters.alteration_value |> String.fromFloat
    , planetaryAlbedo = PlanetaryAlbedoCustom
    , planetaryAlbedoCustomValue = parameters.albedo_value |> String.fromFloat
    , oceanicCarbonSink = OceanicCarbonSinkConstantCustom
    , oceanicCarbonSinkCustomValue = parameters.puit_oce_value |> String.fromFloat
    , vegetationCarbonSink = VegetationCarbonSinkCustom
    , vegetationCarbonSinkCustomValue = parameters.puit_bio_value |> String.fromFloat
    , waterVaporConcentration = WaterVaporConcentrationConstantCustom
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
    , fixed_eau = defaults.fixed_eau
    , fixed_concentration =
        case parametersForm.co2 of
            Co2Constant ->
                True

            Co2SourcesAndSinks ->
                False
    , debranche_biologie = defaults.debranche_biologie
    , fixed_ocean = defaults.fixed_ocean
    , debranche_ocean = defaults.debranche_ocean
    , fixed_albedo = defaults.fixed_albedo
    , rapport_H2O_value = defaults.rapport_H2O_value
    , puit_bio_value = defaults.puit_bio_value
    , puit_oce_value = defaults.puit_oce_value
    , albedo_value = defaults.albedo_value
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
    , alteration_value = defaults.alteration_value
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
    , stockage_biologique_value = defaults.stockage_biologique_value
    }
