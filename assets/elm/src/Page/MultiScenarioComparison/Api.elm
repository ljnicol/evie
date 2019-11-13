module Page.MultiScenarioComparison.Api exposing (..)

import Http
import HttpBuilder
import Page.MultiScenarioComparison.Msg as Msg
import Types.Scenario as TypesScenario


getMultiScenarioComparison : Int -> Cmd Msg.Msg
getMultiScenarioComparison scenarioId =
    "/api/scenario_detail?scenarioId="
        ++ String.fromInt scenarioId
        |> HttpBuilder.get
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetMultiScenarioComparison TypesScenario.multiScenarioComparisonDecoder)
        |> HttpBuilder.request
