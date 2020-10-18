module Ui.Data.ParametersForm exposing
    ( AnthropogenicEmissions(..)
    , BiologicalStorage(..)
    , Co2(..)
    , Co2Concentration(..)
    , ContinentalAlteration(..)
    , CustomChoice
    , CustomChoiceSelected(..)
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
    , earthSunDistance : CustomChoice EarthSunDistance
    , solarPower : CustomChoice SolarPower
    , excentricity : CustomChoice Excentricity
    , obliquity : CustomChoice Obliquity
    , precession : CustomChoice Precession
    , co2 : Co2
    , co2Concentration : CustomChoice Co2Concentration
    , anthropogenicEmissions : CustomChoice AnthropogenicEmissions
    , volcanicEmissions : CustomChoice VolcanicEmissions
    , biologicalStorage : CustomChoice BiologicalStorage
    , continentalAlteration : CustomChoice ContinentalAlteration
    , planetaryAlbedo : CustomChoice PlanetaryAlbedo
    , oceanicCarbonSink : CustomChoice OceanicCarbonSink
    , vegetationCarbonSink : CustomChoice VegetationCarbonSink
    , waterVaporConcentration : CustomChoice WaterVaporConcentration
    }


type alias CustomChoice a =
    { selected : CustomChoiceSelected a
    , customValue : String
    }


type CustomChoiceSelected a
    = Predefined a
    | Custom


type EarthSunDistance
    = EarthSunDistancePresentDay


type SolarPower
    = SolarPowerPresentDay
    | SolarPowerEarthBeginning


type Excentricity
    = ExcentricityPresentDay
    | ExcentricityMinimum
    | ExcentricityMaximum


type Obliquity
    = ObliquityPresentDay
    | ObliquityMinimum
    | ObliquityMaximum


type Precession
    = PrecessionPresentDay
    | PrecessionMinimum
    | PrecessionMaximum


type Co2
    = Co2Constant
    | Co2SourcesAndSinks


type Co2Concentration
    = Co2ConcentrationPresentDay
    | Co2ConcentrationPreIndustrial
    | Co2ConcentrationCretaceous
    | Co2ConcentrationEarthBeginning


type AnthropogenicEmissions
    = AnthropogenicEmissionsNull
    | AnthropogenicEmissionsPresentDay
    | AnthropogenicEmissionsTwicePresentDay


type VolcanicEmissions
    = VolcanicEmissionsPresentDay
    | VolcanicEmissionsEarthBeginning


type BiologicalStorage
    = BiologicalStoragePresentDay
    | BiologicalStorageCarboniferous


type ContinentalAlteration
    = ContinentalAlterationPresentDay


type PlanetaryAlbedo
    = PlanetaryAlbedoComputed
    | PlanetaryAlbedoPresentDay
    | PlanetaryAlbedoPreIndustrial
    | PlanetaryAlbedoSameAsSoil
    | PlanetaryAlbedoSameAsIce


type OceanicCarbonSink
    = OceanicCarbonSinkNeglected
    | OceanicCarbonSinkComputed
    | OceanicCarbonSinkConstantPresentDay


type VegetationCarbonSink
    = VegetationCarbonSinkNeglected
    | VegetationCarbonSinkAsToday


type WaterVaporConcentration
    = WaterVaporConcentrationComputed
    | WaterVaporConcentrationConstantPresentDay
    | WaterVaporConcentrationConstantPreIndustrial


fromParameters : Parameters -> ParametersForm
fromParameters parameters =
    { initialState = parameters.initialState
    , simulationLength = parameters.duration |> Duration.intoYears |> String.fromInt
    , earthSunDistance =
        { selected =
            if parameters.distance_ts_value == 100 then
                Predefined EarthSunDistancePresentDay

            else
                Custom
        , customValue = parameters.distance_ts_value |> String.fromFloat
        }
    , solarPower =
        { selected =
            if parameters.puissance_soleil_value == 100 then
                Predefined SolarPowerPresentDay

            else if parameters.puissance_soleil_value == 70 then
                Predefined SolarPowerEarthBeginning

            else
                Custom
        , customValue = parameters.puissance_soleil_value |> String.fromFloat
        }
    , excentricity =
        { selected =
            if parameters.excentricite_value == 0.0167 then
                Predefined ExcentricityPresentDay

            else if parameters.excentricite_value == 0 then
                Predefined ExcentricityMinimum

            else if parameters.excentricite_value == 0.06 then
                Predefined ExcentricityMaximum

            else
                Custom
        , customValue = parameters.excentricite_value |> String.fromFloat
        }
    , obliquity =
        { selected =
            if parameters.obliquite_value == 23.5 then
                Predefined ObliquityPresentDay

            else if parameters.obliquite_value == 21.8 then
                Predefined ObliquityMinimum

            else if parameters.obliquite_value == 24.4 then
                Predefined ObliquityMaximum

            else
                Custom
        , customValue = parameters.obliquite_value |> String.fromFloat
        }
    , precession =
        { selected =
            if parameters.precession_value == 102.7 then
                Predefined PrecessionPresentDay

            else if parameters.precession_value == 90 then
                Predefined PrecessionMinimum

            else if parameters.precession_value == 270 then
                Predefined PrecessionMaximum

            else
                Custom
        , customValue = parameters.precession_value |> String.fromFloat
        }
    , co2 =
        if parameters.fixed_concentration then
            Co2Constant

        else
            Co2SourcesAndSinks
    , co2Concentration =
        { selected =
            if parameters.coo_concentr_value == 405 then
                Predefined Co2ConcentrationPresentDay

            else if parameters.coo_concentr_value == 280 then
                Predefined Co2ConcentrationPreIndustrial

            else if parameters.coo_concentr_value == 1500 then
                Predefined Co2ConcentrationCretaceous

            else if parameters.coo_concentr_value == 300000 then
                Predefined Co2ConcentrationEarthBeginning

            else
                Custom
        , customValue = parameters.coo_concentr_value |> String.fromFloat
        }
    , anthropogenicEmissions =
        { selected =
            if parameters.emit_anthro_coo_value == 0 then
                Predefined AnthropogenicEmissionsNull

            else if parameters.emit_anthro_coo_value == 8 then
                Predefined AnthropogenicEmissionsPresentDay

            else if parameters.emit_anthro_coo_value == 16 then
                Predefined AnthropogenicEmissionsTwicePresentDay

            else
                Custom
        , customValue = parameters.emit_anthro_coo_value |> String.fromFloat
        }
    , volcanicEmissions =
        { selected =
            if parameters.volcan_value == 0.083 then
                Predefined VolcanicEmissionsPresentDay

            else if parameters.volcan_value == 0.42 then
                Predefined VolcanicEmissionsEarthBeginning

            else
                Custom
        , customValue = parameters.volcan_value |> String.fromFloat
        }
    , biologicalStorage =
        { selected =
            if parameters.stockage_biologique_value == 0 then
                Predefined BiologicalStoragePresentDay

            else if parameters.stockage_biologique_value == 0.71 then
                Predefined BiologicalStorageCarboniferous

            else
                Custom
        , customValue = parameters.stockage_biologique_value |> String.fromFloat
        }
    , continentalAlteration =
        { selected =
            if parameters.alteration_value == 100 then
                Predefined ContinentalAlterationPresentDay

            else
                Custom
        , customValue = parameters.alteration_value |> String.fromFloat
        }
    , planetaryAlbedo =
        { selected =
            if not parameters.fixed_albedo then
                Predefined PlanetaryAlbedoComputed

            else if parameters.albedo_value == 33 then
                Predefined PlanetaryAlbedoPresentDay

            else if parameters.albedo_value == 33 then
                Predefined PlanetaryAlbedoPreIndustrial

            else if parameters.albedo_value == 25 then
                Predefined PlanetaryAlbedoSameAsSoil

            else if parameters.albedo_value == 90 then
                Predefined PlanetaryAlbedoSameAsIce

            else
                Custom
        , customValue = parameters.albedo_value |> String.fromFloat
        }
    , oceanicCarbonSink =
        { selected =
            Debug.log "initial oceanicCarbonSink" <|
                case
                    ( Debug.log "initial debranche_ocean" parameters.debranche_ocean
                    , Debug.log "initial fixed_ocean" parameters.fixed_ocean
                    , truncate <| Debug.log "initial puit_oce_value" parameters.puit_oce_value
                    )
                of
                    ( True, _, _ ) ->
                        Predefined OceanicCarbonSinkNeglected

                    ( False, True, 20 ) ->
                        Predefined OceanicCarbonSinkConstantPresentDay

                    ( False, True, _ ) ->
                        Custom

                    ( False, False, _ ) ->
                        Predefined OceanicCarbonSinkComputed
        , customValue = parameters.puit_oce_value |> String.fromFloat
        }
    , vegetationCarbonSink =
        { selected =
            if parameters.debranche_biologie then
                Predefined VegetationCarbonSinkNeglected

            else if parameters.puit_bio_value == 105.7 then
                Predefined VegetationCarbonSinkAsToday

            else
                Custom
        , customValue = parameters.puit_bio_value |> String.fromFloat
        }
    , waterVaporConcentration =
        { selected =
            if not parameters.fixed_eau then
                Predefined WaterVaporConcentrationComputed

            else if parameters.rapport_H2O_value == 105.7 then
                Predefined WaterVaporConcentrationConstantPresentDay

            else if parameters.rapport_H2O_value == 100 then
                Predefined WaterVaporConcentrationConstantPreIndustrial

            else
                Custom
        , customValue = parameters.rapport_H2O_value |> String.fromFloat
        }
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
        parametersForm.waterVaporConcentration.selected /= Predefined WaterVaporConcentrationComputed
    , fixed_concentration =
        case parametersForm.co2 of
            Co2Constant ->
                True

            Co2SourcesAndSinks ->
                False
    , debranche_biologie =
        parametersForm.vegetationCarbonSink.selected == Predefined VegetationCarbonSinkNeglected
    , fixed_ocean =
        Debug.log "fixed_ocean" <|
            case Debug.log "oceanicCarbonSink" parametersForm.oceanicCarbonSink.selected of
                Predefined OceanicCarbonSinkNeglected ->
                    False

                Predefined OceanicCarbonSinkComputed ->
                    False

                _ ->
                    True
    , debranche_ocean =
        Debug.log "debranche_ocean" <|
            case parametersForm.oceanicCarbonSink.selected of
                Predefined OceanicCarbonSinkNeglected ->
                    True

                _ ->
                    False
    , fixed_albedo =
        case parametersForm.planetaryAlbedo.selected of
            Predefined PlanetaryAlbedoComputed ->
                False

            _ ->
                True
    , rapport_H2O_value =
        parametersForm.waterVaporConcentration.customValue
            |> String.toFloat
            |> Maybe.withDefault defaults.rapport_H2O_value
    , puit_bio_value =
        parametersForm.vegetationCarbonSink.customValue
            |> String.toFloat
            |> Maybe.withDefault defaults.puit_bio_value
    , puit_oce_value =
        Debug.log "puit_oce_value" <|
            case parametersForm.oceanicCarbonSink.selected of
                Predefined OceanicCarbonSinkConstantPresentDay ->
                    20

                _ ->
                    parametersForm.oceanicCarbonSink.customValue
                        |> String.toFloat
                        |> Maybe.withDefault defaults.puit_oce_value
    , albedo_value =
        Maybe.withDefault defaults.albedo_value <|
            case parametersForm.planetaryAlbedo.selected of
                Predefined PlanetaryAlbedoComputed ->
                    Nothing

                Predefined PlanetaryAlbedoPresentDay ->
                    Just 33

                Predefined PlanetaryAlbedoPreIndustrial ->
                    Just 33

                Predefined PlanetaryAlbedoSameAsSoil ->
                    Just 25

                Predefined PlanetaryAlbedoSameAsIce ->
                    Just 90

                Custom ->
                    String.toFloat parametersForm.planetaryAlbedo.customValue
    , coo_concentr_value =
        case parametersForm.co2Concentration.selected of
            Predefined Co2ConcentrationPresentDay ->
                405

            Predefined Co2ConcentrationPreIndustrial ->
                280

            Predefined Co2ConcentrationCretaceous ->
                1500

            Predefined Co2ConcentrationEarthBeginning ->
                300000

            Custom ->
                parametersForm.co2Concentration.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.coo_concentr_value
    , puissance_soleil_value =
        case parametersForm.solarPower.selected of
            Predefined SolarPowerPresentDay ->
                100

            Predefined SolarPowerEarthBeginning ->
                70

            Custom ->
                parametersForm.solarPower.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.puissance_soleil_value
    , distance_ts_value =
        case parametersForm.earthSunDistance.selected of
            Predefined EarthSunDistancePresentDay ->
                100

            Custom ->
                parametersForm.earthSunDistance.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.distance_ts_value
    , obliquite_value =
        case parametersForm.obliquity.selected of
            Predefined ObliquityPresentDay ->
                23.5

            Predefined ObliquityMinimum ->
                21.8

            Predefined ObliquityMaximum ->
                24.4

            Custom ->
                parametersForm.obliquity.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.obliquite_value
    , excentricite_value =
        case parametersForm.excentricity.selected of
            Predefined ExcentricityPresentDay ->
                0.0167

            Predefined ExcentricityMinimum ->
                0

            Predefined ExcentricityMaximum ->
                0.06

            Custom ->
                parametersForm.excentricity.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.excentricite_value
    , precession_value =
        case parametersForm.precession.selected of
            Predefined PrecessionPresentDay ->
                102.7

            Predefined PrecessionMinimum ->
                90

            Predefined PrecessionMaximum ->
                270

            Custom ->
                parametersForm.precession.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.precession_value
    , alteration_value =
        case parametersForm.continentalAlteration.selected of
            Predefined ContinentalAlterationPresentDay ->
                100

            Custom ->
                parametersForm.continentalAlteration.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.alteration_value
    , emit_anthro_coo_value =
        case parametersForm.anthropogenicEmissions.selected of
            Predefined AnthropogenicEmissionsNull ->
                0

            Predefined AnthropogenicEmissionsPresentDay ->
                8

            Predefined AnthropogenicEmissionsTwicePresentDay ->
                16

            Custom ->
                parametersForm.anthropogenicEmissions.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.emit_anthro_coo_value
    , volcan_value =
        case parametersForm.volcanicEmissions.selected of
            Predefined VolcanicEmissionsPresentDay ->
                0.083

            Predefined VolcanicEmissionsEarthBeginning ->
                0.42

            Custom ->
                parametersForm.volcanicEmissions.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.volcan_value
    , stockage_biologique_value =
        case parametersForm.biologicalStorage.selected of
            Predefined BiologicalStoragePresentDay ->
                0

            Predefined BiologicalStorageCarboniferous ->
                0.71

            Custom ->
                parametersForm.biologicalStorage.customValue
                    |> String.toFloat
                    |> Maybe.withDefault defaults.stockage_biologique_value
    }
