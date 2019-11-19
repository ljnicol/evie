module Page.MultiScenarioComparison.Model exposing (..)

import RemoteData
import Types.Scenario as TypesScenario


type alias Model =
    { scenarioIds : List Int
    , multiScenarioComparison : RemoteData.WebData (List TypesScenario.MultiScenarioComparison)
    }


model : Model
model =
    { scenarioIds = []
    , multiScenarioComparison = RemoteData.NotAsked
    }
