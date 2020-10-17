module Main exposing (main)

import Browser
import ClimateSimulation exposing (ClimateSimulation)
import ClimateSimulation.Duration as Duration
import ClimateSimulation.Parameters as Parameters exposing (Parameters)
import ClimateSimulation.State as State
import Color
import Element exposing (Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Ui.Data.ParametersForm as ParametersForm exposing (ParametersForm)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { simulation : ClimateSimulation
    , editing : Maybe ParameterName
    , parametersForm : ParametersForm
    }


type ParameterName
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


init : Model
init =
    let
        parameters =
            Parameters.default
    in
    { simulation =
        ClimateSimulation.run
            { name = "Test"
            , parameters = parameters
            , results = []
            }
    , parametersForm = ParametersForm.fromParameters parameters
    , editing = Just WaterVaporConcentration
    }



-- UPDATE


type Msg
    = NoOp
    | EditParameter (Maybe ParameterName)
    | ChangeInitialState Parameters.InitialState
    | ChangeSimulationLength String
    | ChangeEarthSunDistance ParametersForm.EarthSunDistance
    | ChangeEarthSunDistanceCustomValue String
    | ChangeSolarPower ParametersForm.SolarPower
    | ChangeSolarPowerCustomValue String
    | ChangeExcentricity ParametersForm.Excentricity
    | ChangeExcentricityCustomValue String
    | ChangeObliquity ParametersForm.Obliquity
    | ChangeObliquityCustomValue String
    | ChangePrecession ParametersForm.Precession
    | ChangePrecessionCustomValue String
    | ChangeCo2 ParametersForm.Co2
    | ChangeCo2Concentration ParametersForm.Co2Concentration
    | ChangeCo2ConcentrationCustomValue String
    | ChangeAnthropogenicEmissions ParametersForm.AnthropogenicEmissions
    | ChangeAnthropogenicEmissionsCustomValue String
    | ChangeVolcanicEmissions ParametersForm.VolcanicEmissions
    | ChangeVolcanicEmissionsCustomValue String
    | ChangeBiologicalStorage ParametersForm.BiologicalStorage
    | ChangeBiologicalStorageCustomValue String
    | ChangeContinentalAlteration ParametersForm.ContinentalAlteration
    | ChangeContinentalAlterationCustomValue String
    | ChangePlanetaryAlbedo ParametersForm.PlanetaryAlbedo
    | ChangePlanetaryAlbedoCustomValue String
    | ChangeOceanicCarbonSink ParametersForm.OceanicCarbonSink
    | ChangeOceanicCarbonSinkCustomValue String
    | ChangeVegetationCarbonSink ParametersForm.VegetationCarbonSink
    | ChangeVegetationCarbonSinkCustomValue String
    | ChangeWaterVaporConcentration ParametersForm.WaterVaporConcentration
    | ChangeWaterVaporConcentrationCustomValue String


update : Msg -> Model -> Model
update msg model =
    let
        parametersForm =
            model.parametersForm
    in
    case msg of
        NoOp ->
            model

        EditParameter editing ->
            { model | editing = editing }

        ChangeInitialState initialState ->
            { model | parametersForm = { parametersForm | initialState = initialState } }
                |> updateSimulation

        ChangeSimulationLength simulationLength ->
            { model | parametersForm = { parametersForm | simulationLength = simulationLength } }
                |> updateSimulation

        ChangeEarthSunDistance earthSunDistance ->
            { model | parametersForm = { parametersForm | earthSunDistance = earthSunDistance } }
                |> updateSimulation

        ChangeEarthSunDistanceCustomValue value ->
            { model | parametersForm = { parametersForm | earthSunDistanceCustomValue = value } }
                |> updateSimulation

        ChangeSolarPower value ->
            { model | parametersForm = { parametersForm | solarPower = value } }
                |> updateSimulation

        ChangeSolarPowerCustomValue value ->
            { model | parametersForm = { parametersForm | solarPowerCustomValue = value } }
                |> updateSimulation

        ChangeExcentricity excentricity ->
            { model | parametersForm = { parametersForm | excentricity = excentricity } }
                |> updateSimulation

        ChangeExcentricityCustomValue value ->
            { model | parametersForm = { parametersForm | excentricityCustomValue = value } }
                |> updateSimulation

        ChangeObliquity value ->
            { model | parametersForm = { parametersForm | obliquity = value } }
                |> updateSimulation

        ChangeObliquityCustomValue value ->
            { model | parametersForm = { parametersForm | obliquityCustomValue = value } }
                |> updateSimulation

        ChangePrecession value ->
            { model | parametersForm = { parametersForm | precession = value } }
                |> updateSimulation

        ChangePrecessionCustomValue value ->
            { model | parametersForm = { parametersForm | precessionCustomValue = value } }
                |> updateSimulation

        ChangeCo2 value ->
            { model | parametersForm = { parametersForm | co2 = value } }
                |> updateSimulation

        ChangeCo2Concentration value ->
            { model | parametersForm = { parametersForm | co2Concentration = value } }
                |> updateSimulation

        ChangeCo2ConcentrationCustomValue value ->
            { model | parametersForm = { parametersForm | co2ConcentrationCustomValue = value } }
                |> updateSimulation

        ChangeAnthropogenicEmissions value ->
            { model | parametersForm = { parametersForm | anthropogenicEmissions = value } }
                |> updateSimulation

        ChangeAnthropogenicEmissionsCustomValue value ->
            { model | parametersForm = { parametersForm | anthropogenicEmissionsCustomValue = value } }
                |> updateSimulation

        ChangeVolcanicEmissions value ->
            { model | parametersForm = { parametersForm | volcanicEmissions = value } }
                |> updateSimulation

        ChangeVolcanicEmissionsCustomValue value ->
            { model | parametersForm = { parametersForm | volcanicEmissionsCustomValue = value } }
                |> updateSimulation

        ChangeBiologicalStorage value ->
            { model | parametersForm = { parametersForm | biologicalStorage = value } }
                |> updateSimulation

        ChangeBiologicalStorageCustomValue value ->
            { model | parametersForm = { parametersForm | biologicalStorageCustomValue = value } }
                |> updateSimulation

        ChangeContinentalAlteration value ->
            { model | parametersForm = { parametersForm | continentalAlteration = value } }
                |> updateSimulation

        ChangeContinentalAlterationCustomValue value ->
            { model | parametersForm = { parametersForm | continentalAlterationCustomValue = value } }
                |> updateSimulation

        ChangePlanetaryAlbedo value ->
            { model | parametersForm = { parametersForm | planetaryAlbedo = value } }
                |> updateSimulation

        ChangePlanetaryAlbedoCustomValue value ->
            { model | parametersForm = { parametersForm | planetaryAlbedoCustomValue = value } }
                |> updateSimulation

        ChangeOceanicCarbonSink value ->
            { model | parametersForm = { parametersForm | oceanicCarbonSink = value } }
                |> updateSimulation

        ChangeOceanicCarbonSinkCustomValue value ->
            { model | parametersForm = { parametersForm | oceanicCarbonSinkCustomValue = value } }
                |> updateSimulation

        ChangeVegetationCarbonSink value ->
            { model | parametersForm = { parametersForm | vegetationCarbonSink = value } }
                |> updateSimulation

        ChangeVegetationCarbonSinkCustomValue value ->
            { model | parametersForm = { parametersForm | vegetationCarbonSinkCustomValue = value } }
                |> updateSimulation

        ChangeWaterVaporConcentration value ->
            { model | parametersForm = { parametersForm | waterVaporConcentration = value } }
                |> updateSimulation

        ChangeWaterVaporConcentrationCustomValue value ->
            { model | parametersForm = { parametersForm | waterVaporConcentrationCustomValue = value } }
                |> updateSimulation


updateSimulation : Model -> Model
updateSimulation model =
    let
        simulation =
            model.simulation

        parameters =
            ParametersForm.toParameters model.parametersForm simulation.parameters
    in
    { model
        | simulation =
            ClimateSimulation.run { simulation | parameters = parameters }
    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.height Element.fill
        ]
    <|
        column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ el [ Element.width Element.fill ] <|
                viewHeader model.simulation
            , row
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ viewParameterList model
                , viewEditing model
                , viewCharts model.simulation
                ]
            ]


viewHeader : ClimateSimulation -> Element Msg
viewHeader simulation =
    row
        [ Element.width Element.fill
        , Background.color colorBlack
        ]
        [ el
            [ Element.centerX
            , Element.padding 10
            , Font.color colorWhite
            ]
            (text ("Climate Simulation: " ++ simulation.name))
        ]


viewParameterList : Model -> Element Msg
viewParameterList model =
    let
        parameters : Parameters
        parameters =
            model.simulation.parameters

        selectedParameter : Maybe ParameterName
        selectedParameter =
            model.editing
    in
    column
        [ Element.width <| Element.fillPortion 1
        , Element.height Element.fill
        , Background.color colorLightGray
        ]
        [ column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 20
            , Element.spacing 20
            , Element.scrollbarY
            ]
            [ viewParameterSummary InitialState selectedParameter "📅" "Initial state" <|
                case parameters.initialState of
                    Parameters.PreIndustrial ->
                        "pre-industrial"

                    Parameters.Now ->
                        "present-day"
            , viewParameterSummary SimulationLength selectedParameter "⏱" "Simulation length" <|
                String.fromInt (Duration.intoYears parameters.duration)
                    ++ " years"
            , viewParameterSection "Astronomical parameters"
                [ viewParameterSummary EarthSunDistance selectedParameter "☀️" "Earth-Sun distance" <|
                    String.fromFloat parameters.distance_ts_value
                        ++ "% of present-day"
                , viewParameterSummary SolarPower selectedParameter "🔋" "Solar power" <|
                    String.fromFloat parameters.puissance_soleil_value
                        ++ "% of present-day"
                , viewParameterSummary Excentricity selectedParameter "🌍" "Excentricity" <|
                    String.fromFloat parameters.excentricite_value
                , viewParameterSummary Obliquity selectedParameter "🌍" "Obliquity" <|
                    String.fromFloat parameters.obliquite_value
                        ++ "º"
                , viewParameterSummary Precession selectedParameter "🌍" "Precession" <|
                    String.fromFloat parameters.precession_value
                        ++ "º"
                ]
            , viewParameterSection "CO2 emissions" <|
                List.concat
                    [ [ viewParameterSummary Co2 selectedParameter "☁️" "CO2" <|
                            if parameters.fixed_concentration then
                                "constant"

                            else
                                "sources & sinks"
                      ]
                    , case model.parametersForm.co2 of
                        ParametersForm.Co2Constant ->
                            [ viewParameterSummary Co2Concentration selectedParameter "💨" "CO2 concentration" <|
                                String.fromFloat parameters.coo_concentr_value
                                    ++ " ppm"
                            ]

                        ParametersForm.Co2SourcesAndSinks ->
                            [ viewParameterSummary AnthropogenicEmissions selectedParameter "👨" "Anthropogenic emissions" <|
                                String.fromFloat parameters.emit_anthro_coo_value
                                    ++ " GtC/year"
                            , viewParameterSummary VolcanicEmissions selectedParameter "🌋" "Volcanic emissions" <|
                                String.fromFloat parameters.volcan_value
                                    ++ " GtC/year"
                            , viewParameterSummary BiologicalStorage selectedParameter "🛢" "Biological storage" <|
                                String.fromFloat parameters.stockage_biologique_value
                                    ++ " Mt/year/ppm"
                            , viewParameterSummary ContinentalAlteration selectedParameter "⛰" "Continental alteration" <|
                                String.fromFloat parameters.alteration_value
                                    ++ "% relatively to present-day"
                            ]
                    ]
            , viewParameterSection "Climate feedbacks"
                [ viewParameterSummary PlanetaryAlbedo selectedParameter "✨" "Planetary albedo" <|
                    if parameters.fixed_albedo then
                        String.fromFloat parameters.albedo_value ++ "%"

                    else
                        "depends on temperature"
                , viewParameterSummary OceanicCarbonSink selectedParameter "🌊" "Oceanic carbon sink" <|
                    if parameters.debranche_ocean then
                        "neglected"

                    else if parameters.fixed_ocean then
                        String.fromFloat parameters.puit_oce_value ++ "%"

                    else
                        "depends on temperature"
                , viewParameterSummary VegetationCarbonSink selectedParameter "🌳" "Vegetation carbon sink" <|
                    if parameters.debranche_biologie then
                        "neglected"

                    else
                        String.fromFloat parameters.puit_bio_value ++ "%"
                , viewParameterSummary WaterVaporConcentration selectedParameter "💧" "Water vapor concentration" <|
                    if parameters.fixed_eau then
                        String.fromFloat parameters.rapport_H2O_value
                            ++ "% of present-day"

                    else
                        "depends on temperature"
                ]
            ]
        ]


viewParameterSection : String -> List (Element Msg) -> Element Msg
viewParameterSection title contents =
    column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ el
            [ Element.width Element.fill
            , Element.paddingXY 0 10
            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
            , Border.color colorGray
            , Font.size 14
            ]
          <|
            text (title ++ ":")
        , column [ Element.spacing 20 ]
            contents
        ]


viewParameterSummary : ParameterName -> Maybe ParameterName -> String -> String -> String -> Element Msg
viewParameterSummary parameterName selectedParameter icon label value =
    let
        isSelected =
            selectedParameter == Just parameterName

        selectedBackgroundColor =
            if isSelected then
                Background.color colorWhite

            else
                noAttribute

        toggleEditingOnClick =
            Element.Events.onClick
                (EditParameter
                    (if isSelected then
                        Nothing

                     else
                        Just parameterName
                    )
                )
    in
    Element.row
        [ Element.spacing 5
        , selectedBackgroundColor
        , toggleEditingOnClick
        ]
        [ el [ Element.alignTop ] (text icon)
        , Element.wrappedRow [ Element.width Element.fill, Element.spacing 10 ]
            [ text (label ++ ":")
            , el [ Font.color colorDarkGray ]
                (text value)
            ]
        ]


viewEditing : Model -> Element Msg
viewEditing { editing, parametersForm } =
    case editing of
        Nothing ->
            text ""

        Just InitialState ->
            viewEditingInitialState parametersForm

        Just SimulationLength ->
            viewEditingSimulationLength parametersForm

        Just EarthSunDistance ->
            viewEditingEarthSunDistance parametersForm

        Just SolarPower ->
            viewEditingSolarPower parametersForm

        Just Excentricity ->
            viewEditingExcentricity parametersForm

        Just Obliquity ->
            viewEditingObliquity parametersForm

        Just Precession ->
            viewEditingPrecession parametersForm

        Just Co2 ->
            viewEditingCo2 parametersForm

        Just Co2Concentration ->
            viewEditingCo2Concentration parametersForm

        Just AnthropogenicEmissions ->
            viewEditingAnthropogenicEmissions parametersForm

        Just VolcanicEmissions ->
            viewEditingVolcanicEmissions parametersForm

        Just BiologicalStorage ->
            viewEditingBiologicalStorage parametersForm

        Just ContinentalAlteration ->
            viewEditingContinentalAlteration parametersForm

        Just PlanetaryAlbedo ->
            viewEditingPlanetaryAlbedo parametersForm

        Just OceanicCarbonSink ->
            viewEditingOceanicCarbonSink parametersForm

        Just VegetationCarbonSink ->
            viewEditingVegetationCarbonSink parametersForm

        Just WaterVaporConcentration ->
            viewEditingWaterVaporConcentration parametersForm


viewEditingInitialState : ParametersForm -> Element Msg
viewEditingInitialState parametersForm =
    viewEditingParameter
        { title = "Initial state"
        , form =
            [ Input.radio []
                { onChange = ChangeInitialState
                , options =
                    [ Input.option Parameters.PreIndustrial (text "Pre-industrial")
                    , Input.option Parameters.Now (text "Present-day")
                    ]
                , selected = Just parametersForm.initialState
                , label = Input.labelHidden "Initial state"
                }
            ]
        }


viewEditingSimulationLength : ParametersForm -> Element Msg
viewEditingSimulationLength parametersForm =
    viewEditingParameter
        { title = ""
        , form =
            [ Input.text []
                { onChange = ChangeSimulationLength
                , text = parametersForm.simulationLength
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Simulation length (in years)"
                }
            ]
        }


viewEditingEarthSunDistance : ParametersForm -> Element Msg
viewEditingEarthSunDistance parametersForm =
    viewEditingParameter
        { title = "Earth-Sun distance"
        , form =
            [ Input.radio []
                { onChange = ChangeEarthSunDistance
                , options =
                    [ Input.option ParametersForm.EarthSunDistancePresentDay (text "Present-day distance (100%)")
                    , Input.option ParametersForm.EarthSunDistanceCustom (text "Other value")
                    ]
                , selected = Just parametersForm.earthSunDistance
                , label = Input.labelHidden "Earth-Sun distance"
                }
            , Input.text []
                { onChange = ChangeEarthSunDistanceCustomValue
                , text = parametersForm.earthSunDistanceCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Earth-Sun distance (in percentage of the present-day distance)"
                }
            ]
        }


viewEditingSolarPower : ParametersForm -> Element Msg
viewEditingSolarPower parametersForm =
    viewEditingParameter
        { title = "Solar Power"
        , form =
            [ Input.radio []
                { onChange = ChangeSolarPower
                , options =
                    [ Input.option ParametersForm.SolarPowerPresentDay (text "Present-day power (100%)")
                    , Input.option ParametersForm.SolarPowerEarthBeginning (text "Same as at the beginning of the Earth history (70%)")
                    , Input.option ParametersForm.SolarPowerCustom (text "Other value")
                    ]
                , selected = Just parametersForm.solarPower
                , label = Input.labelHidden "Solar Power"
                }
            , Input.text []
                { onChange = ChangeSolarPowerCustomValue
                , text = parametersForm.solarPowerCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Solar power (in percentage of the present-day distance)"
                }
            ]
        }


viewEditingExcentricity : ParametersForm -> Element Msg
viewEditingExcentricity parametersForm =
    viewEditingParameter
        { title = "Excentricity"
        , form =
            [ Input.radio []
                { onChange = ChangeExcentricity
                , options =
                    [ Input.option ParametersForm.ExcentricityPresentDay (text "Present-day excentricity (0.0167)")
                    , Input.option ParametersForm.ExcentricityMinimum (text "Minimum value (0)")
                    , Input.option ParametersForm.ExcentricityMaximum (text "Maximum value (0.06)")
                    , Input.option ParametersForm.ExcentricityCustom (text "OtherValue")
                    ]
                , selected = Just parametersForm.excentricity
                , label = Input.labelHidden "Excentricity"
                }
            , Input.text []
                { onChange = ChangeExcentricityCustomValue
                , text = parametersForm.excentricityCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Excentricity"
                }
            ]
        }


viewEditingObliquity : ParametersForm -> Element Msg
viewEditingObliquity parametersForm =
    let
        value =
            parametersForm.obliquity
    in
    viewEditingParameter
        { title = "Obliquity"
        , form =
            [ Input.radio []
                { onChange = ChangeObliquity
                , options =
                    [ Input.option ParametersForm.ObliquityPresentDay (text "Present-day obliquity (23.5º)")
                    , Input.option ParametersForm.ObliquityMinimum (text "Minimum value (21.8º)")
                    , Input.option ParametersForm.ObliquityMaximum (text "Maximum value (24.4º)")
                    , Input.option ParametersForm.ObliquityCustom (text "Other value")
                    ]
                , selected = Just value
                , label = Input.labelHidden "Obliquity"
                }
            , Input.text []
                { onChange = ChangeObliquityCustomValue
                , text = parametersForm.obliquityCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Obliquity (in º)"
                }
            ]
        }


viewEditingPrecession : ParametersForm -> Element Msg
viewEditingPrecession parametersForm =
    let
        value =
            parametersForm.precession
    in
    viewEditingParameter
        { title = "Precession"
        , form =
            [ Input.radio []
                { onChange = ChangePrecession
                , options =
                    [ Input.option ParametersForm.PrecessionPresentDay (text "Present-day precession (102.7º)")
                    , Input.option ParametersForm.PrecessionMinimum (text "Minimum value (90º)")
                    , Input.option ParametersForm.PrecessionMaximum (text "Maximum value (270º)")
                    , Input.option ParametersForm.PrecessionCustom (text "Other value")
                    ]
                , selected = Just value
                , label = Input.labelHidden "Precession"
                }
            , Input.text []
                { onChange = ChangePrecessionCustomValue
                , text = parametersForm.precessionCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Precession (in º)"
                }
            ]
        }


viewEditingCo2 : ParametersForm -> Element Msg
viewEditingCo2 parametersForm =
    viewEditingParameter
        { title = "CO2 emissions"
        , form =
            [ Input.radio []
                { onChange = ChangeCo2
                , options =
                    [ Input.option ParametersForm.Co2Constant (text "Constant CO2 concentration")
                    , Input.option ParametersForm.Co2SourcesAndSinks (text "Set the CO2 sources and sinks")
                    ]
                , selected = Just parametersForm.co2
                , label = Input.labelHidden "CO2 emissions"
                }
            ]
        }


viewEditingCo2Concentration : ParametersForm -> Element Msg
viewEditingCo2Concentration parametersForm =
    viewEditingParameter
        { title = "CO2 concentration"
        , form =
            [ Input.radio []
                { onChange = ChangeCo2Concentration
                , options =
                    [ Input.option ParametersForm.Co2ConcentrationPresentDay (text "Present-day value (405 ppm)")
                    , Input.option ParametersForm.Co2ConcentrationPreIndustrial (text "Pre-industrial value (280 ppm)")
                    , Input.option ParametersForm.Co2ConcentrationCretaceous (text "Cretaceous value (1500 ppm)")
                    , Input.option ParametersForm.Co2ConcentrationEarthBeginning (text "Same as at the beginning of the Earth history (300\u{00A0}000 ppm)")
                    , Input.option ParametersForm.Co2ConcentrationCustom (text "Other value")
                    ]
                , selected = Just parametersForm.co2Concentration
                , label = Input.labelHidden "CO2 concentration"
                }
            , Input.text []
                { onChange = ChangeCo2ConcentrationCustomValue
                , text = parametersForm.co2ConcentrationCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "CO2 concentration (in ppm)"
                }
            ]
        }


viewEditingAnthropogenicEmissions : ParametersForm -> Element Msg
viewEditingAnthropogenicEmissions parametersForm =
    viewEditingParameter
        { title = "Anthropogenic emissions"
        , form =
            [ Input.radio []
                { onChange = ChangeAnthropogenicEmissions
                , options =
                    [ Input.option ParametersForm.AnthropogenicEmissionsNull (text "Null (0 GtC/year)")
                    , Input.option ParametersForm.AnthropogenicEmissionsPresentDay (text "Present-day value (8 GtC/year)")
                    , Input.option ParametersForm.AnthropogenicEmissionsTwicePresentDay (text "Twice the present-day value (16 GtC/year)")
                    , Input.option ParametersForm.AnthropogenicEmissionsCustom (text "Other value")
                    ]
                , selected = Just parametersForm.anthropogenicEmissions
                , label = Input.labelHidden "Anthropogenic emissions"
                }
            , Input.text []
                { onChange = ChangeAnthropogenicEmissionsCustomValue
                , text = parametersForm.anthropogenicEmissionsCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Anthropogenic emissions (in GtC/year)"
                }
            ]
        }


viewEditingVolcanicEmissions : ParametersForm -> Element Msg
viewEditingVolcanicEmissions parametersForm =
    viewEditingParameter
        { title = "Volcanic and oceanic ridge activity"
        , form =
            [ Input.radio []
                { onChange = ChangeVolcanicEmissions
                , options =
                    [ Input.option ParametersForm.VolcanicEmissionsPresentDay (text "Same as present-day (0.083 GtC/year)")
                    , Input.option ParametersForm.VolcanicEmissionsEarthBeginning (text "Same as at the beginning of the Earth history (0.42 GtC/year)")
                    , Input.option ParametersForm.VolcanicEmissionsCustom (text "Other value")
                    ]
                , selected = Just parametersForm.volcanicEmissions
                , label = Input.labelHidden "Volcanic emissions"
                }
            , Input.text []
                { onChange = ChangeVolcanicEmissionsCustomValue
                , text = parametersForm.volcanicEmissionsCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Volcanic emissions (in GtC/year)"
                }
            ]
        }


viewEditingBiologicalStorage : ParametersForm -> Element Msg
viewEditingBiologicalStorage parametersForm =
    viewEditingParameter
        { title = "Biological storage"
        , form =
            [ Input.radio []
                { onChange = ChangeBiologicalStorage
                , options =
                    [ Input.option ParametersForm.BiologicalStoragePresentDay (text "Same as present-day (0 Mt/year/ppm)")
                    , Input.option ParametersForm.BiologicalStorageCarboniferous (text "Same as during the Carboniferous (0.71 Mt/year/ppm)")
                    , Input.option ParametersForm.BiologicalStorageCustom (text "Other value")
                    ]
                , selected = Just parametersForm.biologicalStorage
                , label = Input.labelHidden "Biological storage"
                }
            , Input.text []
                { onChange = ChangeBiologicalStorageCustomValue
                , text = parametersForm.biologicalStorageCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Biological storage (in Mt/year/ppm)"
                }
            ]
        }


viewEditingContinentalAlteration : ParametersForm -> Element Msg
viewEditingContinentalAlteration parametersForm =
    viewEditingParameter
        { title = "Continental alteration"
        , form =
            [ Input.radio []
                { onChange = ChangeContinentalAlteration
                , options =
                    [ Input.option ParametersForm.ContinentalAlterationPresentDay (text "Same as present-day (100%)")
                    , Input.option ParametersForm.ContinentalAlterationCustom (text "Other value")
                    ]
                , selected = Just parametersForm.continentalAlteration
                , label = Input.labelHidden "Continental alteration"
                }
            , Input.text []
                { onChange = ChangeContinentalAlterationCustomValue
                , text = parametersForm.continentalAlterationCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Continental alteration (in %)"
                }
            ]
        }


viewEditingPlanetaryAlbedo : ParametersForm -> Element Msg
viewEditingPlanetaryAlbedo parametersForm =
    viewEditingParameter
        { title = "Planetary albedo"
        , form =
            [ Input.radio []
                { onChange = ChangePlanetaryAlbedo
                , options =
                    [ Input.option ParametersForm.PlanetaryAlbedoComputed (text "Computed as a function of temperature, permitting the feedback")
                    , Input.option ParametersForm.PlanetaryAlbedoPresentDay (text "Constant as its present-day value (33%)")
                    , Input.option ParametersForm.PlanetaryAlbedoPreIndustrial (text "Constant as its pre-industrial value (33%)")
                    , Input.option ParametersForm.PlanetaryAlbedoSameAsSoil (text "Constant at the value of a soil (25%)")
                    , Input.option ParametersForm.PlanetaryAlbedoSameAsIce (text "Constant at the value of ice (90%)")
                    , Input.option ParametersForm.PlanetaryAlbedoCustom (text "Constant at another value")
                    ]
                , selected = Just parametersForm.planetaryAlbedo
                , label = Input.labelHidden "Planetary albedo"
                }
            , Input.text []
                { onChange = ChangePlanetaryAlbedoCustomValue
                , text = parametersForm.planetaryAlbedoCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Planetary albedo (in %)"
                }
            ]
        }


viewEditingOceanicCarbonSink : ParametersForm -> Element Msg
viewEditingOceanicCarbonSink parametersForm =
    viewEditingParameter
        { title = "Oceanic carbon sink"
        , form =
            [ Input.radio []
                { onChange = ChangeOceanicCarbonSink
                , options =
                    [ Input.option ParametersForm.OceanicCarbonSinkNeglected (text "Neglect the oceanic sink")
                    , Input.option ParametersForm.OceanicCarbonSinkComputed (text "The oceanic carbon sink is computed as a function of temperature")
                    , Input.option ParametersForm.OceanicCarbonSinkConstantPresentDay (text "The oceanic carbon sink does not depend on temperature and remains as today")
                    , Input.option ParametersForm.OceanicCarbonSinkConstantCustom (text "Other value")
                    ]
                , selected = Just parametersForm.oceanicCarbonSink
                , label = Input.labelHidden "Oceanic carbon sink"
                }
            , Input.text []
                { onChange = ChangeOceanicCarbonSinkCustomValue
                , text = parametersForm.oceanicCarbonSinkCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Oceanic carbon sink (in %)"
                }
            ]
        }


viewEditingVegetationCarbonSink : ParametersForm -> Element Msg
viewEditingVegetationCarbonSink parametersForm =
    viewEditingParameter
        { title = "Vegetation carbon sink"
        , form =
            [ Input.radio []
                { onChange = ChangeVegetationCarbonSink
                , options =
                    [ Input.option ParametersForm.VegetationCarbonSinkNeglected (text "Neglect the CO2 fluxes associated with the vegetation")
                    , Input.option ParametersForm.VegetationCarbonSinkAsToday (text "The vegetation mops up 35% of anthropogenic CO2 emissions, as today")
                    , Input.option ParametersForm.VegetationCarbonSinkCustom (text "Other value")
                    ]
                , selected = Just parametersForm.vegetationCarbonSink
                , label = Input.labelHidden "Vegetation carbon sink"
                }
            , Input.text []
                { onChange = ChangeVegetationCarbonSinkCustomValue
                , text = parametersForm.vegetationCarbonSinkCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Vegetation carbon sink (in %)"
                }
            ]
        }


viewEditingWaterVaporConcentration : ParametersForm -> Element Msg
viewEditingWaterVaporConcentration parametersForm =
    viewEditingParameter
        { title = "Water vapor atmospheric concentration"
        , form =
            [ Input.radio []
                { onChange = ChangeWaterVaporConcentration
                , options =
                    [ Input.option ParametersForm.WaterVaporConcentrationComputed (text "Computed as a function of temperature")
                    , Input.option ParametersForm.WaterVaporConcentrationConstantPresentDay (text "Constant at the present-day value (105.7%)")
                    , Input.option ParametersForm.WaterVaporConcentrationConstantPreIndustrial (text "Constant at the pre-industrial value (100%)")
                    , Input.option ParametersForm.WaterVaporConcentrationConstantCustom (text "Other value")
                    ]
                , selected = Just parametersForm.waterVaporConcentration
                , label = Input.labelHidden "Water vapor concentration (in %)"
                }
            , Input.text []
                { onChange = ChangeWaterVaporConcentrationCustomValue
                , text = parametersForm.waterVaporConcentrationCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Water vapor concentration (in %)"
                }
            ]
        }


viewEditingParameter : { title : String, form : List (Element Msg) } -> Element Msg
viewEditingParameter { title, form } =
    column
        [ Element.height Element.fill
        , Element.width (Element.fillPortion 1)
        , Background.color colorGray
        ]
        [ el [] (text title)
        , column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            form
        ]


viewCharts : ClimateSimulation -> Element Msg
viewCharts sv =
    Element.wrappedRow
        [ Element.width <| Element.fillPortion 2
        , Element.height Element.fill
        , Element.padding 20
        , Element.scrollbars
        ]
        [ viewChart
            { id = "sea-level-chart"
            , title = "Sea level relatively to pre-industrial (m)"
            , dots = []
            , timeInYears = identity
            , value = identity
            }
        , viewChart
            { id = "albedo-chart"
            , title = "Albedo %"
            , dots = sv.results
            , timeInYears = State.timeInYears
            , value = State.albedoPercentage
            }
        , viewChart
            { id = "co2-concentration-chart"
            , title = "CO2 concentration (ppm)"
            , dots = sv.results
            , timeInYears = State.timeInYears
            , value = State.co2Concentration
            }
        , viewChart
            { id = "ice-cap-latitude-chart"
            , title = "Latitude down to which the Northern ice sheet extends (º)"
            , dots = []
            , timeInYears = identity
            , value = identity
            }
        , viewChart
            { id = "emissions-chart"
            , title = "Emissions (Gt/year of Carbon)"
            , dots = ClimateSimulation.co2Emissions sv
            , timeInYears = .timeInYears
            , value = .emissionInGtPerYear
            }
        , viewChart
            { id = "temperature-chart"
            , title = "Temperature (ºC)"
            , dots = sv.results
            , timeInYears = State.timeInYears
            , value = State.celsiusTemperature
            }
        ]


type alias ChartConfig a =
    { id : String
    , title : String
    , dots : List a
    , timeInYears : a -> Float
    , value : a -> Float
    }


viewChart : ChartConfig a -> Element Msg
viewChart config =
    column []
        [ el [ Element.centerX ] (text config.title)
        , Element.html <|
            LineChart.viewCustom
                { y = Axis.default 300 "" config.value
                , x = Axis.default 550 "Time (years)" config.timeInYears
                , container =
                    Container.styled config.id
                        [ ( "font-family", "monospace" )
                        , ( "font-size", "10px" )
                        ]
                , interpolation = Interpolation.default
                , intersection = Intersection.default
                , legends = Legends.none
                , events = Events.default
                , junk = Junk.default
                , grid = Grid.default
                , area = Area.default
                , line = Line.wider 2
                , dots = Dots.default
                }
                [ LineChart.line Color.red Dots.none "" config.dots
                ]
        ]


noAttribute : Element.Attribute msg
noAttribute =
    Element.htmlAttribute (Html.Attributes.class "")



-- COLORS


colorBlack : Element.Color
colorBlack =
    Element.rgb255 0 0 0


colorDarkGray : Element.Color
colorDarkGray =
    Element.rgb255 100 100 100


colorGray : Element.Color
colorGray =
    Element.rgb255 200 200 200


colorLightGray : Element.Color
colorLightGray =
    Element.rgb255 230 230 230


colorWhite : Element.Color
colorWhite =
    Element.rgb255 255 255 255
