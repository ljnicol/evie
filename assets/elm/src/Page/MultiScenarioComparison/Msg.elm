module Page.MultiScenarioComparison.Msg exposing (..)

import Http
import Types.Scenario as TypesScenario


type Msg
    = LoadMultiScenarioComparison (List Int)
    | HandleGetMultiScenarioComparison (Result Http.Error TypesScenario.MultiScenarioComparison)
