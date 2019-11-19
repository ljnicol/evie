module Page.MultiScenarioComparison.Api exposing (..)

import Http
import HttpBuilder
import Page.MultiScenarioComparison.Msg as Msg
import Types.Scenario as TypesScenario


getMultiScenarioComparison : String -> List Int -> Cmd Msg.Msg
getMultiScenarioComparison year scenarioIds =
    "/api/multi_scenario_comparison?"
        ++ "year="
        ++ year
        ++ "&"
        ++ String.join "&" (List.map (\s -> "scenarioId=" ++ String.fromInt s) scenarioIds)
        |> HttpBuilder.get
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetMultiScenarioComparison TypesScenario.decodeListMultiScenario)
        |> HttpBuilder.request
