module Page.ScenariosList.Msg exposing (..)

import Http
import Types.Scenario as TypesScenario


type Msg
    = LoadScenariosList
    | OpenScenarioDetail (Maybe String) TypesScenario.Scenario
    | HandleGetScenariosList (Result Http.Error (List TypesScenario.Scenario))
    | SelectScenario TypesScenario.Scenario Bool
    | ShowMultiScenarioTable
    | ShowMultiScenarioDetail
