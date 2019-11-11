module Page.ScenarioDetail.Api exposing (..)

import Http
import HttpBuilder
import Page.ScenarioDetail.Msg as Msg
import Page.ScenarioDetail.Types as Types


getScenarioDetail : Cmd Msg.Msg
getScenarioDetail =
    HttpBuilder.get "/app/history"
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson Msg.HandleGetScenarioDetail Types.decodeScenarioDetailScenarioDetail)
        |> HttpBuilder.request
