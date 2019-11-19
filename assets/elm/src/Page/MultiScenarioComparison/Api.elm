module Page.MultiScenarioComparison.Api exposing (..)

import Http
import HttpBuilder
import Page.MultiScenarioComparison.Msg as Msg
import Types.Scenario as TypesScenario


getMultiScenarioComparison : List Int -> Cmd Msg.Msg
getMultiScenarioComparison scenarioIds =
    "/api/multi_scenario_comparison?"
        ++ String.join "&" (List.map (\s -> "scenarioId=" ++ String.fromInt s) scenarioIds)
        ++ "&year=2019"
        |> HttpBuilder.get
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetMultiScenarioComparison TypesScenario.decodeListMultiScenario)
        |> HttpBuilder.request
