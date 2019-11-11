module Page.ScenarioDetail.Msg exposing (..)

import Http
import Page.ScenarioDetail.Types as Types


type Msg
    = LoadScenarioDetail
    | OpenReport Types.ReportScenarioDetail
    | HandleGetScenarioDetail (Result Http.Error (List Types.ReportScenarioDetail))
