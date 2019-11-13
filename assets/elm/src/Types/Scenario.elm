module Types.Scenario exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


type alias Scenario =
    { scenario_id : Int
    , scenario_name : String
    , scenario_description : String
    , scenario_assumptions : String
    , scenario_spatial_table : String
    , scenario_years : List Int
    }


scenarioDecoder : Decode.Decoder Scenario
scenarioDecoder =
    Decode.succeed Scenario
        |> DecodePipeline.required "scenario_id" Decode.int
        |> DecodePipeline.required "scenario_name" Decode.string
        |> DecodePipeline.required "scenario_description" Decode.string
        |> DecodePipeline.required "scenario_assumptions" Decode.string
        |> DecodePipeline.required "scenario_spatial_table" Decode.string
        |> DecodePipeline.required "scenario_years" (Decode.list Decode.int)


decodeListScenario : Decode.Decoder (List Scenario)
decodeListScenario =
    Decode.list scenarioDecoder


type alias ScenarioDetail =
    { scenario : Scenario
    , metrics : List MetricData
    }


scenarioDetailDecoder : Decode.Decoder ScenarioDetail
scenarioDetailDecoder =
    Decode.succeed ScenarioDetail
        |> DecodePipeline.required "scenario" scenarioDecoder
        |> DecodePipeline.required "metrics" decodeListMetricData


type alias MetricData =
    { metric_data_id : Int
    , metric_name : String
    , metric_description : String
    , metric_year : Int
    , metric_value : Float
    , metric_spatial_table_column : String
    }


metricDataDecoder : Decode.Decoder MetricData
metricDataDecoder =
    Decode.succeed MetricData
        |> DecodePipeline.required "metric_data_id" Decode.int
        |> DecodePipeline.required "metric_name" Decode.string
        |> DecodePipeline.required "metric_description" Decode.string
        |> DecodePipeline.required "metric_year" Decode.int
        |> DecodePipeline.required "metric_value" Decode.float
        |> DecodePipeline.required "metric_spatial_table_column" Decode.string


decodeListMetricData : Decode.Decoder (List MetricData)
decodeListMetricData =
    Decode.list metricDataDecoder
