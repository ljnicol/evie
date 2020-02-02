module Page.MultiScenarioMap.Api exposing (..)

import Http
import HttpBuilder
import Page.MultiScenarioMap.Msg as Msg
import Types.Metric as TypesMetric


getMultiScenarioMetrics : Int -> Int -> Cmd Msg.Msg
getMultiScenarioMetrics scenarioId1 scenarioId2 =
    "/api/multi_scenario_metrics?"
        ++ "scenarioId1="
        ++ String.fromInt scenarioId1
        ++ "&scenarioId2="
        ++ String.fromInt scenarioId2
        |> HttpBuilder.get
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetMultiScenarioMetrics TypesMetric.decodeMetrics)
        |> HttpBuilder.request
