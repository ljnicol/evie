module Page.ScenarioDetail.Model exposing (..)

import Page.ScenarioDetail.Types as ScenarioDetailTypes
import RemoteData


type alias Model =
    { reportsScenarioDetail : RemoteData.WebData (List ScenarioDetailTypes.ReportScenarioDetail)
    }


model : Model
model =
    { reportsScenarioDetail = RemoteData.NotAsked
    }
