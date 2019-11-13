module Page.ScenariosList.Model exposing (..)

import RemoteData
import Types.Scenario as TypesScenario


type alias Model =
    { scenariosList : RemoteData.WebData (List TypesScenario.Scenario)
    }


model : Model
model =
    { scenariosList = RemoteData.NotAsked
    }
