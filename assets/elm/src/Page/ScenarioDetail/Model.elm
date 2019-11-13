module Page.ScenarioDetail.Model exposing (..)

import RemoteData
import Types.Scenario as TypesScenario


type alias Model =
    { scenarioDetail : RemoteData.WebData TypesScenario.ScenarioDetail
    }


model : Model
model =
    { scenarioDetail = RemoteData.NotAsked
    }
