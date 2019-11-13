module Page.MultiScenarioComparison.Msg exposing (..)

import Http
import Types.Scenario as TypesScenario


type Msg
    = LoadMultiScenarioComparison
    | HandleGetMultiScenarioComparison (Result Http.Error TypesScenario.MultiScenarioComparison)
