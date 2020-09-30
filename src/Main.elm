module Main exposing (main)

import Browser
import ClimateSimulation exposing (SimulationValues)
import ClimateSimulation.PhysicsConstants as PhysicsConstants
import Color
import Html exposing (Html, h2, li, text, ul)
import Html.Attributes exposing (class)
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
    { simulation : SimulationValues
    }


init : Model
init =
    { simulation =
        { simulation_name = "Test"

        -- CONFIG
        , initialState = ClimateSimulation.Now

        -- , annee_debut = 2020
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

        -- RESULTS
        , results = []
        }
            |> ClimateSimulation.simulate
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



--mVIEW


view : Model -> Html Msg
view model =
    Html.div [ class "container" ]
        [ h2 [] [ text "Température (ºC)" ]
        , chart model.simulation
        , viewDebug model.simulation
        ]


viewDebug sv =
    sv.results
        |> List.map (.zT >> String.fromFloat >> text >> List.singleton >> li [])
        |> ul []


chart sv =
    LineChart.viewCustom
        { y = Axis.default 450 "" celsiusTemperature
        , x = Axis.default 700 "Temps (années)" (.t >> (+) (ClimateSimulation.startYear sv.initialState) >> toFloat)
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.none
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line =
            -- Try out these different configs!
            -- Line.default
            Line.wider 2

        -- For making the line change based on whether it's hovered, see Events.elm!
        , dots = Dots.default
        }
        [ LineChart.line Color.red Dots.none "" sv.results
        ]


celsiusTemperature : ClimateSimulation.State -> Float
celsiusTemperature state =
    state.zT - PhysicsConstants.tKelvin
