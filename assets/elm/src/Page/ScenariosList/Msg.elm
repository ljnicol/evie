module Page.ScenariosList.Msg exposing (..)

import Http
import Page.ScenariosList.Types as Types


type Msg
    = LoadScenariosList
    | OpenScenario Types.Scenario
    | HandleGetScenariosList (Result Http.Error (List Types.Scenario))
