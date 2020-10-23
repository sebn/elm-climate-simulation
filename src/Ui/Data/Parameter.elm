module Ui.Data.Parameter exposing
    ( Parameter(..)
    , icon
    , name
    )

import Element exposing (Element)


type Parameter
    = InitialState
    | SimulationLength
    | EarthSunDistance
    | SolarPower
    | Excentricity
    | Obliquity
    | Precession
    | Co2
    | Co2Concentration
    | AnthropogenicEmissions
    | VolcanicEmissions
    | BiologicalStorage
    | ContinentalAlteration
    | PlanetaryAlbedo
    | OceanicCarbonSink
    | VegetationCarbonSink
    | WaterVaporConcentration


icon : Parameter -> Element msg
icon parameter =
    Element.text <|
        case parameter of
            InitialState ->
                "📅"

            SimulationLength ->
                "⏱"

            EarthSunDistance ->
                "☀️"

            SolarPower ->
                "🔋"

            Excentricity ->
                "🌍"

            Obliquity ->
                "🌍"

            Precession ->
                "🌍"

            Co2 ->
                "☁️"

            Co2Concentration ->
                "💨"

            AnthropogenicEmissions ->
                "👨"

            VolcanicEmissions ->
                "🌋"

            BiologicalStorage ->
                "🛢"

            ContinentalAlteration ->
                "⛰"

            PlanetaryAlbedo ->
                "✨"

            OceanicCarbonSink ->
                "🌊"

            VegetationCarbonSink ->
                "🌳"

            WaterVaporConcentration ->
                "💧"


name : Parameter -> String
name parameter =
    case parameter of
        InitialState ->
            "Initial state"

        SimulationLength ->
            "Simulation length"

        EarthSunDistance ->
            "Earth-Sun distance"

        SolarPower ->
            "Solar power"

        Excentricity ->
            "Excentricity"

        Obliquity ->
            "Obliquity"

        Precession ->
            "Precession"

        Co2 ->
            "CO2"

        Co2Concentration ->
            "CO2 concentration"

        AnthropogenicEmissions ->
            "Anthropogenic emissions"

        VolcanicEmissions ->
            "Volcanic emissions"

        BiologicalStorage ->
            "Biological storage"

        ContinentalAlteration ->
            "Continental alteration"

        PlanetaryAlbedo ->
            "Planetary albedo"

        OceanicCarbonSink ->
            "Oceanic carbon sink"

        VegetationCarbonSink ->
            "Vegetation carbon sink"

        WaterVaporConcentration ->
            "Water vapor concentration"
