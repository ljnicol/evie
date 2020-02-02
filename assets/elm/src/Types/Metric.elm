module Types.Metric exposing (..)

import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


type alias MetricData =
    { metric_id : Int
    , metric_name : String
    , metric_description : String
    , metric_year : String
    , metric_value : Float
    }


metricDataDecoder : Decode.Decoder MetricData
metricDataDecoder =
    Decode.succeed MetricData
        |> DecodePipeline.required "metric_id" Decode.int
        |> DecodePipeline.required "metric_name" Decode.string
        |> DecodePipeline.required "metric_description" Decode.string
        |> DecodePipeline.required "metric_year" Decode.string
        |> DecodePipeline.required "metric_value" Decode.float


decodeListMetricData : Decode.Decoder (List MetricData)
decodeListMetricData =
    Decode.list metricDataDecoder


type alias MetricAccessor =
    ( String, String )


toMetricAccessor : Dict.Dict String MetricData -> List MetricAccessor
toMetricAccessor d =
    List.map (\( k, v ) -> ( k, v.metric_name )) (Dict.toList d)


type alias Metric =
    { id : Int
    , name : String
    }


decodeMetric : Decode.Decoder Metric
decodeMetric =
    Decode.succeed Metric
        |> DecodePipeline.required "id" Decode.int
        |> DecodePipeline.required "name" Decode.string


decodeMetrics : Decode.Decoder (List Metric)
decodeMetrics =
    Decode.list decodeMetric
