module Page.ScenariosList.Api exposing (..)

import Http
import HttpBuilder
import Page.ScenariosList.Msg as Msg
import Types.Scenario as TypesScenario


getScenariosList : Cmd Msg.Msg
getScenariosList =
    HttpBuilder.get "/api/scenarios"
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetScenariosList TypesScenario.decodeListScenario)
        |> HttpBuilder.request
