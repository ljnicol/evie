module Page.ScenariosList.Msg exposing (..)

import Http
import Types.Scenario as TypesScenario


type Msg
    = LoadScenariosList
    | OpenScenarioDetail TypesScenario.Scenario
    | HandleGetScenariosList (Result Http.Error (List TypesScenario.Scenario))
    | SelectScenario TypesScenario.Scenario Bool
    | ShowMultiScenario
