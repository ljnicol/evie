module Page.ScenariosList.Msg exposing (..)

import Http
import Types.Scenario as TypesScenario


type Msg
    = LoadScenariosList
    | OpenMultiScenarioComparison TypesScenario.Scenario
    | HandleGetScenariosList (Result Http.Error (List TypesScenario.Scenario))
