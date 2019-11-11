module Page.ScenariosList.Api exposing (..)

import Http
import HttpBuilder
import Page.ScenariosList.Msg as Msg
import Page.ScenariosList.Types as Types


getScenariosList : Cmd Msg.Msg
getScenariosList =
    HttpBuilder.get "/api/scenarios"
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetScenariosList Types.decodeListScenario)
        |> HttpBuilder.request
