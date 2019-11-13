module Page.ScenarioDetail.Msg exposing (..)

import Http
import Types.Scenario as TypesScenario


type Msg
    = LoadScenarioDetail
    | HandleGetScenarioDetail (Result Http.Error TypesScenario.ScenarioDetail)
