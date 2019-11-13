module Page.MultiScenarioComparison.Model exposing (..)

import RemoteData
import Types.Scenario as TypesScenario


type alias Model =
    { multiScenarioComparison : RemoteData.WebData TypesScenario.MultiScenarioComparison
    }


model : Model
model =
    { multiScenarioComparison = RemoteData.NotAsked
    }
