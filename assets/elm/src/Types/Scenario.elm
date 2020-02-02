module Types.Scenario exposing (..)

import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Types.Metric as TypesMetric


type alias Scenario =
    { scenario_id : Int
    , scenario_name : String
    , scenario_description : String
    , scenario_assumptions : String
    , scenario_years : List String
    , selected : Bool
    }


scenarioDecoder : Decode.Decoder Scenario
scenarioDecoder =
    Decode.succeed Scenario
        |> DecodePipeline.required "scenario_id" Decode.int
        |> DecodePipeline.required "scenario_name" Decode.string
        |> DecodePipeline.required "scenario_description" Decode.string
        |> DecodePipeline.required "scenario_assumptions" Decode.string
        |> DecodePipeline.required "scenario_years" (Decode.list Decode.string)
        |> DecodePipeline.hardcoded False


decodeListScenario : Decode.Decoder (List Scenario)
decodeListScenario =
    Decode.list scenarioDecoder


type alias MultiScenarioComparison =
    { scenario : Scenario
    , metric_data : Dict.Dict String TypesMetric.MetricData
    , year : String
    }


multiScenarioComparisonDecoder : Decode.Decoder MultiScenarioComparison
multiScenarioComparisonDecoder =
    Decode.succeed MultiScenarioComparison
        |> DecodePipeline.required "scenario" scenarioDecoder
        |> DecodePipeline.required "metric_data" (Decode.dict TypesMetric.metricDataDecoder)
        |> DecodePipeline.required "year" Decode.string


decodeListMultiScenario : Decode.Decoder (List MultiScenarioComparison)
decodeListMultiScenario =
    Decode.list multiScenarioComparisonDecoder
