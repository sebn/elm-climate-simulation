module Main exposing (main)

import Browser
import ClimateSimulation exposing (ClimateSimulation)
import ClimateSimulation.Duration as Duration exposing (Duration)
import ClimateSimulation.Parameters as Parameters exposing (Parameters)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import ClimateSimulation.State as State
import Color
import Element exposing (Element, column, el, paddingEach, row, text)
import Element.Background as Background
import Element.Border as Border
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
    }


init : Model
init =
    { simulation =
        ClimateSimulation.run
            { name = "Test"
            , parameters = defaultParameters
            , results = []
            }
    }


defaultParameters : Parameters
defaultParameters =
    { initialState = Parameters.Now

    -- , annee_debut = 2020
    , duration = Duration.fromYears 500
    , fixed_eau = False
    , fixed_concentration = False
    , debranche_biologie = False
    , fixed_ocean = False
    , debranche_ocean = False
    , fixed_albedo = False
    , rapport_H2O_value = 105.71900000000001
    , puit_bio_value = 35
    , puit_oce_value = 20
    , albedo_value = 0
    , coo_concentr_value = PhysicsConstants.concentration_coo_actuel
    , puissance_soleil_value = 100
    , distance_ts_value = 100
    , obliquite_value = 23.5
    , excentricite_value = 0.0167
    , precession_value = 102.7
    , alteration_value = 100
    , emit_anthro_coo_value = 8
    , volcan_value = 0.083
    , stockage_biologique_value = 0
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



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
            [ row
                [ Element.width Element.fill
                , Background.color <| Element.rgb255 0 0 0
                ]
                [ el
                    [ Element.centerX
                    , Element.padding 10
                    , Font.color <| Element.rgb255 255 255 255
                    ]
                  <|
                    text <|
                        "Climate Simulation: "
                            ++ model.simulation.name
                ]
            , row
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ viewParameterList model.simulation.parameters
                , viewCharts model.simulation
                ]
            ]


viewParameterList : Parameters -> Element Msg
viewParameterList parameters =
    column
        [ Element.width <| Element.fillPortion 1
        , Element.height Element.fill
        , Background.color <| Element.rgb255 230 230 230
        ]
        [ column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 20
            , Element.spacing 20
            , Element.scrollbarY
            ]
            [ viewParameter "📅" "Initial state" <|
                case parameters.initialState of
                    Parameters.PreIndustrial ->
                        "pre-industrial"

                    Parameters.Now ->
                        "present-day"
            , viewParameter "⏱" "Simulation length" <|
                String.fromInt (Duration.intoYears parameters.duration)
                    ++ " years"
            , viewSection "Astronomical parameters"
                [ viewParameter "☀️" "Earth-Sun distance" <|
                    String.fromFloat parameters.distance_ts_value
                        ++ "% of present-day"
                , viewParameter "🔋" "Solar power" <|
                    String.fromFloat parameters.puissance_soleil_value
                        ++ "% of present-day"
                , viewParameter "🌍" "Excentricity" <|
                    String.fromFloat parameters.excentricite_value
                , viewParameter "🌍" "Obliquity" <|
                    String.fromFloat parameters.obliquite_value
                        ++ "º"
                , viewParameter "🌍" "Precession" <|
                    String.fromFloat parameters.precession_value
                        ++ "º"
                ]
            , viewSection "CO2 emissions"
                [ viewParameter "🌫" "CO2 concentration" <|
                    if parameters.fixed_concentration then
                        String.fromFloat parameters.coo_concentr_value

                    else
                        "depends on sources & sinks"
                , viewParameter "👨" "Anthropogenic emissions" <|
                    String.fromFloat parameters.emit_anthro_coo_value
                        ++ " GtC/year"
                , viewParameter "🌋" "Volcanic emissions" <|
                    String.fromFloat parameters.volcan_value
                        ++ " GtC/year"
                , viewParameter "🛢" "Biological storage" <|
                    String.fromFloat parameters.stockage_biologique_value
                        ++ " Mt/year/ppm"
                , viewParameter "⛰" "Continental alteration" <|
                    String.fromFloat parameters.alteration_value
                        ++ "% relatively to present-day"
                ]
            , viewSection "Climate feedbacks"
                [ viewParameter "✨" "Planetary albedo" <|
                    if parameters.fixed_albedo then
                        String.fromFloat parameters.albedo_value ++ "%"

                    else
                        "depends on temperature"
                , viewParameter "🌊" "Oceanic carbon sink" <|
                    if parameters.debranche_ocean then
                        "neglected"

                    else if parameters.fixed_ocean then
                        String.fromFloat parameters.puit_oce_value ++ "%"

                    else
                        "depends on temperature"
                , viewParameter "🌳" "Vegetation carbon sink" <|
                    if parameters.debranche_biologie then
                        "neglected"

                    else
                        String.fromFloat parameters.puit_bio_value ++ "%"
                , viewParameter "☁️" "Water vapor concentration" <|
                    if parameters.fixed_eau then
                        String.fromFloat parameters.rapport_H2O_value
                            ++ "% of present-day"

                    else
                        "depends on temperature"
                ]
            ]
        ]


viewSection : String -> List (Element Msg) -> Element Msg
viewSection title contents =
    column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ el
            [ Element.width Element.fill
            , Element.paddingXY 0 10
            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
            , Border.color (Element.rgb255 200 200 200)
            , Font.size 14
            ]
          <|
            text (title ++ ":")
        , column [ Element.spacing 20 ]
            contents
        ]


viewParameter : String -> String -> String -> Element Msg
viewParameter icon label value =
    Element.row [ Element.spacing 5 ]
        [ el [ Element.alignTop ] (text icon)
        , Element.wrappedRow [ Element.width Element.fill, Element.spacing 10 ]
            [ text (label ++ ":")
            , el [ Font.color (Element.rgb255 100 100 100) ]
                (text value)
            ]
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
