module Page.ScenariosList.Model exposing (..)

import Page.ScenariosList.Types as ScenariosListTypes
import RemoteData


type alias Model =
    { scenariosList : RemoteData.WebData (List ScenariosListTypes.Scenario)
    }


model : Model
model =
    { scenariosList = RemoteData.NotAsked
    }
