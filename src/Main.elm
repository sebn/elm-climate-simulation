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
import Ui.Data.Parameter as Parameter exposing (Parameter)
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
    , editing : Maybe Parameter
    , parametersForm : ParametersForm
    }


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
    , editing = Nothing
    }



-- UPDATE


type Msg
    = EditParameter (Maybe Parameter)
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
            , Element.clip
            ]
            [ el [ Element.width Element.fill ] <|
                viewHeader model.simulation
            , row
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.clip
                ]
                [ el
                    [ Element.width
                        (Element.fill
                            |> Element.maximum 450
                        )
                    , Element.height Element.fill
                    ]
                    (viewParameterList model)
                , case model.editing of
                    Nothing ->
                        text ""

                    Just parameterName ->
                        el
                            [ Element.width
                                (Element.fill
                                    |> Element.maximum 350
                                )
                            , Element.height Element.fill
                            ]
                            (viewEditing parameterName model.parametersForm)
                , el
                    [ Element.width <| Element.fill
                    , Element.height Element.fill
                    ]
                    (viewCharts model.simulation)
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

        selectedParameter : Maybe Parameter
        selectedParameter =
            model.editing
    in
    column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color colorLightGray
        ]
        [ column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.paddingXY 0 20
            , Element.clipX
            , Element.scrollbarY
            ]
            [ viewParameterSummary Parameter.InitialState selectedParameter <|
                case parameters.initialState of
                    Parameters.PreIndustrial ->
                        "pre-industrial"

                    Parameters.Now ->
                        "present-day"
            , viewParameterSummary Parameter.SimulationLength selectedParameter <|
                String.fromInt (Duration.intoYears parameters.duration)
                    ++ " years"
            , viewParameterSection "Astronomical parameters"
                [ viewParameterSummary Parameter.EarthSunDistance selectedParameter <|
                    String.fromFloat parameters.distance_ts_value
                        ++ "% of present-day"
                , viewParameterSummary Parameter.SolarPower selectedParameter <|
                    String.fromFloat parameters.puissance_soleil_value
                        ++ "% of present-day"
                , viewParameterSummary Parameter.Excentricity selectedParameter <|
                    String.fromFloat parameters.excentricite_value
                , viewParameterSummary Parameter.Obliquity selectedParameter <|
                    String.fromFloat parameters.obliquite_value
                        ++ "º"
                , viewParameterSummary Parameter.Precession selectedParameter <|
                    String.fromFloat parameters.precession_value
                        ++ "º"
                ]
            , viewParameterSection "CO2 emissions" <|
                List.concat
                    [ [ viewParameterSummary Parameter.Co2 selectedParameter <|
                            if parameters.fixed_concentration then
                                "constant"

                            else
                                "sources & sinks"
                      ]
                    , case model.parametersForm.co2 of
                        ParametersForm.Co2Constant ->
                            [ viewParameterSummary Parameter.Co2Concentration selectedParameter <|
                                String.fromFloat parameters.coo_concentr_value
                                    ++ " ppm"
                            ]

                        ParametersForm.Co2SourcesAndSinks ->
                            [ viewParameterSummary Parameter.AnthropogenicEmissions selectedParameter <|
                                String.fromFloat parameters.emit_anthro_coo_value
                                    ++ " GtC/year"
                            , viewParameterSummary Parameter.VolcanicEmissions selectedParameter <|
                                String.fromFloat parameters.volcan_value
                                    ++ " GtC/year"
                            , viewParameterSummary Parameter.BiologicalStorage selectedParameter <|
                                String.fromFloat parameters.stockage_biologique_value
                                    ++ " Mt/year/ppm"
                            , viewParameterSummary Parameter.ContinentalAlteration selectedParameter <|
                                String.fromFloat parameters.alteration_value
                                    ++ "% relatively to present-day"
                            ]
                    ]
            , viewParameterSection "Climate feedbacks"
                [ viewParameterSummary Parameter.PlanetaryAlbedo selectedParameter <|
                    if parameters.fixed_albedo then
                        String.fromFloat parameters.albedo_value ++ "%"

                    else
                        "depends on temperature"
                , viewParameterSummary Parameter.OceanicCarbonSink selectedParameter <|
                    if parameters.debranche_ocean then
                        "neglected"

                    else if parameters.fixed_ocean then
                        String.fromFloat parameters.puit_oce_value ++ "%"

                    else
                        "depends on temperature"
                , viewParameterSummary Parameter.VegetationCarbonSink selectedParameter <|
                    if parameters.debranche_biologie then
                        "neglected"

                    else
                        String.fromFloat parameters.puit_bio_value ++ "%"
                , viewParameterSummary Parameter.WaterVaporConcentration selectedParameter <|
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
        ]
        [ el
            [ Element.width Element.fill
            , Element.paddingEach
                { top = 20, right = 10, bottom = 0, left = 10 }
            ]
          <|
            el
                [ Element.width Element.fill
                , Element.paddingXY 0 10
                , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                , Border.color colorGray
                , Font.size 14
                ]
            <|
                text (title ++ ":")
        , column
            [ Element.width Element.fill
            ]
            contents
        ]


viewParameterSummary : Parameter -> Maybe Parameter -> String -> Element Msg
viewParameterSummary parameter selectedParameter value =
    let
        isSelected =
            selectedParameter == Just parameter

        highlightedWhen condition =
            if condition then
                Background.color colorWhite

            else
                Background.color colorLightGray

        toggleEditingOnClick =
            Element.Events.onClick
                (EditParameter
                    (if isSelected then
                        Nothing

                     else
                        Just parameter
                    )
                )
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 5
        , Element.padding 10
        , highlightedWhen isSelected
        , Element.pointer
        , Element.mouseOver
            [ highlightedWhen True
            ]
        , toggleEditingOnClick
        ]
        [ el [ Element.alignTop ] (Parameter.icon parameter)
        , Element.wrappedRow [ Element.width Element.fill, Element.spacing 10 ]
            [ text (Parameter.name parameter ++ ":")
            , el [ Font.color colorDarkGray ]
                (text value)
            ]
        ]


viewEditing : Parameter -> ParametersForm -> Element Msg
viewEditing parameter parametersForm =
    case parameter of
        Parameter.InitialState ->
            viewEditingInitialState parametersForm

        Parameter.SimulationLength ->
            viewEditingSimulationLength parametersForm

        Parameter.EarthSunDistance ->
            viewEditingEarthSunDistance parametersForm

        Parameter.SolarPower ->
            viewEditingSolarPower parametersForm

        Parameter.Excentricity ->
            viewEditingExcentricity parametersForm

        Parameter.Obliquity ->
            viewEditingObliquity parametersForm

        Parameter.Precession ->
            viewEditingPrecession parametersForm

        Parameter.Co2 ->
            viewEditingCo2 parametersForm

        Parameter.Co2Concentration ->
            viewEditingCo2Concentration parametersForm

        Parameter.AnthropogenicEmissions ->
            viewEditingAnthropogenicEmissions parametersForm

        Parameter.VolcanicEmissions ->
            viewEditingVolcanicEmissions parametersForm

        Parameter.BiologicalStorage ->
            viewEditingBiologicalStorage parametersForm

        Parameter.ContinentalAlteration ->
            viewEditingContinentalAlteration parametersForm

        Parameter.PlanetaryAlbedo ->
            viewEditingPlanetaryAlbedo parametersForm

        Parameter.OceanicCarbonSink ->
            viewEditingOceanicCarbonSink parametersForm

        Parameter.VegetationCarbonSink ->
            viewEditingVegetationCarbonSink parametersForm

        Parameter.WaterVaporConcentration ->
            viewEditingWaterVaporConcentration parametersForm


viewEditingInitialState : ParametersForm -> Element Msg
viewEditingInitialState parametersForm =
    viewEditingParameter
        { title = "Initial state"
        , form =
            [ viewPredefinedOptions
                { onChange = ChangeInitialState
                , options =
                    [ predefinedOption Parameters.PreIndustrial "Pre-industrial"
                    , predefinedOption Parameters.Now "Present-day"
                    ]
                , selected = parametersForm.initialState
                , label = "Initial state"
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
            [ viewPredefinedOptions
                { onChange = ChangeEarthSunDistance
                , options =
                    [ predefinedOption ParametersForm.EarthSunDistancePresentDay "Present-day distance (100%)"
                    , predefinedOption ParametersForm.EarthSunDistanceCustom "Other value"
                    ]
                , selected = parametersForm.earthSunDistance
                , label = "Earth-Sun distance"
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
            [ viewPredefinedOptions
                { onChange = ChangeSolarPower
                , options =
                    [ predefinedOption ParametersForm.SolarPowerPresentDay "Present-day power (100%)"
                    , predefinedOption ParametersForm.SolarPowerEarthBeginning "Same as at the beginning of the Earth history (70%)"
                    , predefinedOption ParametersForm.SolarPowerCustom "Other value"
                    ]
                , selected = parametersForm.solarPower
                , label = "Solar Power"
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
            [ viewPredefinedOptions
                { onChange = ChangeExcentricity
                , options =
                    [ predefinedOption ParametersForm.ExcentricityPresentDay "Present-day excentricity (0.0167)"
                    , predefinedOption ParametersForm.ExcentricityMinimum "Minimum value (0)"
                    , predefinedOption ParametersForm.ExcentricityMaximum "Maximum value (0.06)"
                    , predefinedOption ParametersForm.ExcentricityCustom "OtherValue"
                    ]
                , selected = parametersForm.excentricity
                , label = "Excentricity"
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
            [ viewPredefinedOptions
                { onChange = ChangeObliquity
                , options =
                    [ predefinedOption ParametersForm.ObliquityPresentDay "Present-day obliquity (23.5º)"
                    , predefinedOption ParametersForm.ObliquityMinimum "Minimum value (21.8º)"
                    , predefinedOption ParametersForm.ObliquityMaximum "Maximum value (24.4º)"
                    , predefinedOption ParametersForm.ObliquityCustom "Other value"
                    ]
                , selected = value
                , label = "Obliquity"
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
            [ viewPredefinedOptions
                { onChange = ChangePrecession
                , options =
                    [ predefinedOption ParametersForm.PrecessionPresentDay "Present-day precession (102.7º)"
                    , predefinedOption ParametersForm.PrecessionMinimum "Minimum value (90º)"
                    , predefinedOption ParametersForm.PrecessionMaximum "Maximum value (270º)"
                    , predefinedOption ParametersForm.PrecessionCustom "Other value"
                    ]
                , selected = value
                , label = "Precession"
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
            [ viewPredefinedOptions
                { onChange = ChangeCo2
                , options =
                    [ predefinedOption ParametersForm.Co2Constant "Constant CO2 concentration"
                    , predefinedOption ParametersForm.Co2SourcesAndSinks "Set the CO2 sources and sinks"
                    ]
                , selected = parametersForm.co2
                , label = "CO2 emissions"
                }
            ]
        }


viewEditingCo2Concentration : ParametersForm -> Element Msg
viewEditingCo2Concentration parametersForm =
    viewEditingParameter
        { title = "CO2 concentration"
        , form =
            [ viewPredefinedOptions
                { onChange = ChangeCo2Concentration
                , options =
                    [ predefinedOption ParametersForm.Co2ConcentrationPresentDay "Present-day value (405 ppm)"
                    , predefinedOption ParametersForm.Co2ConcentrationPreIndustrial "Pre-industrial value (280 ppm)"
                    , predefinedOption ParametersForm.Co2ConcentrationCretaceous "Cretaceous value (1500 ppm)"
                    , predefinedOption ParametersForm.Co2ConcentrationEarthBeginning "Same as at the beginning of the Earth history (300\u{00A0}000 ppm)"
                    , predefinedOption ParametersForm.Co2ConcentrationCustom "Other value"
                    ]
                , selected = parametersForm.co2Concentration
                , label = "CO2 concentration"
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
            [ viewPredefinedOptions
                { onChange = ChangeAnthropogenicEmissions
                , options =
                    [ predefinedOption ParametersForm.AnthropogenicEmissionsNull "Null (0 GtC/year)"
                    , predefinedOption ParametersForm.AnthropogenicEmissionsPresentDay "Present-day value (8 GtC/year)"
                    , predefinedOption ParametersForm.AnthropogenicEmissionsTwicePresentDay "Twice the present-day value (16 GtC/year)"
                    , predefinedOption ParametersForm.AnthropogenicEmissionsCustom "Other value"
                    ]
                , selected = parametersForm.anthropogenicEmissions
                , label = "Anthropogenic emissions"
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
            [ viewPredefinedOptions
                { onChange = ChangeVolcanicEmissions
                , options =
                    [ predefinedOption ParametersForm.VolcanicEmissionsPresentDay "Same as present-day (0.083 GtC/year)"
                    , predefinedOption ParametersForm.VolcanicEmissionsEarthBeginning "Same as at the beginning of the Earth history (0.42 GtC/year)"
                    , predefinedOption ParametersForm.VolcanicEmissionsCustom "Other value"
                    ]
                , selected = parametersForm.volcanicEmissions
                , label = "Volcanic emissions"
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
            [ viewPredefinedOptions
                { onChange = ChangeBiologicalStorage
                , options =
                    [ predefinedOption ParametersForm.BiologicalStoragePresentDay "Same as present-day (0 Mt/year/ppm)"
                    , predefinedOption ParametersForm.BiologicalStorageCarboniferous "Same as during the Carboniferous (0.71 Mt/year/ppm)"
                    , predefinedOption ParametersForm.BiologicalStorageCustom "Other value"
                    ]
                , selected = parametersForm.biologicalStorage
                , label = "Biological storage"
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
            [ viewPredefinedOptions
                { onChange = ChangeContinentalAlteration
                , options =
                    [ predefinedOption ParametersForm.ContinentalAlterationPresentDay "Same as present-day (100%)"
                    , predefinedOption ParametersForm.ContinentalAlterationCustom "Other value"
                    ]
                , selected = parametersForm.continentalAlteration
                , label = "Continental alteration"
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
            [ viewPredefinedOptions
                { onChange = ChangePlanetaryAlbedo
                , options =
                    [ predefinedOption ParametersForm.PlanetaryAlbedoComputed "Computed as a function of temperature, permitting the feedback"
                    , predefinedOption ParametersForm.PlanetaryAlbedoPresentDay "Constant as its present-day value (33%)"
                    , predefinedOption ParametersForm.PlanetaryAlbedoPreIndustrial "Constant as its pre-industrial value (33%)"
                    , predefinedOption ParametersForm.PlanetaryAlbedoSameAsSoil "Constant at the value of a soil (25%)"
                    , predefinedOption ParametersForm.PlanetaryAlbedoSameAsIce "Constant at the value of ice (90%)"
                    , predefinedOption ParametersForm.PlanetaryAlbedoCustom "Constant at another value"
                    ]
                , selected = parametersForm.planetaryAlbedo
                , label = "Planetary albedo"
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
            [ viewPredefinedOptions
                { onChange = ChangeOceanicCarbonSink
                , options =
                    [ predefinedOption ParametersForm.OceanicCarbonSinkNeglected "Neglect the oceanic sink"
                    , predefinedOption ParametersForm.OceanicCarbonSinkComputed "The oceanic carbon sink is computed as a function of temperature"
                    , predefinedOption ParametersForm.OceanicCarbonSinkConstantPresentDay "The oceanic carbon sink does not depend on temperature and remains as today"
                    , predefinedOption ParametersForm.OceanicCarbonSinkConstantCustom "Other value"
                    ]
                , selected = parametersForm.oceanicCarbonSink
                , label = "Oceanic carbon sink"
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
            [ viewPredefinedOptions
                { onChange = ChangeVegetationCarbonSink
                , options =
                    [ predefinedOption ParametersForm.VegetationCarbonSinkNeglected "Neglect the CO2 fluxes associated with the vegetation"
                    , predefinedOption ParametersForm.VegetationCarbonSinkAsToday "The vegetation mops up 35% of anthropogenic CO2 emissions, as today"
                    , predefinedOption ParametersForm.VegetationCarbonSinkCustom "Other value"
                    ]
                , selected = parametersForm.vegetationCarbonSink
                , label = "Vegetation carbon sink"
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
            [ viewPredefinedOptions
                { onChange = ChangeWaterVaporConcentration
                , options =
                    [ predefinedOption ParametersForm.WaterVaporConcentrationComputed "Computed as a function of temperature"
                    , predefinedOption ParametersForm.WaterVaporConcentrationConstantPresentDay "Constant at the present-day value (105.7%)"
                    , predefinedOption ParametersForm.WaterVaporConcentrationConstantPreIndustrial "Constant at the pre-industrial value (100%)"
                    , predefinedOption ParametersForm.WaterVaporConcentrationConstantCustom "Other value"
                    ]
                , selected = parametersForm.waterVaporConcentration
                , label = "Water vapor concentration (in %)"
                }
            , Input.text []
                { onChange = ChangeWaterVaporConcentrationCustomValue
                , text = parametersForm.waterVaporConcentrationCustomValue
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Water vapor concentration (in %)"
                }
            ]
        }


viewPredefinedOptions : { onChange : value -> msg, options : List (Input.Option value msg), selected : value, label : String } -> Element msg
viewPredefinedOptions { onChange, options, selected, label } =
    Input.radio []
        { onChange = onChange
        , options = options
        , selected = Just selected
        , label = Input.labelHidden label
        }


predefinedOption : value -> String -> Input.Option value Msg
predefinedOption value label =
    Input.option value (Element.row [] [ text label ])


viewEditingParameter : { title : String, form : List (Element Msg) } -> Element Msg
viewEditingParameter { title, form } =
    column
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.paddingXY 20 30
        , Element.scrollbarY
        , Background.color colorGray
        ]
        [ el [] (text title)
        , column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clipX
            ]
            form
        ]


viewCharts : ClimateSimulation -> Element Msg
viewCharts sv =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 0 30
        , Element.clipX
        , Element.scrollbarY
        ]
    <|
        Element.wrappedRow
            [ Element.width Element.fill
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
