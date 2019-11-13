module Page.ScenarioDetail.Api exposing (..)

import Http
import HttpBuilder
import Page.ScenarioDetail.Msg as Msg
import Types.Scenario as TypesScenario


getScenarioDetail : Int -> Cmd Msg.Msg
getScenarioDetail scenarioId =
    "/api/scenario_detail?scenarioId="
        ++ String.fromInt scenarioId
        |> HttpBuilder.get
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetScenarioDetail TypesScenario.scenarioDetailDecoder)
        |> HttpBuilder.request
