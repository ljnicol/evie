module Page.MultiScenarioMap.Model exposing (..)

import RemoteData
import Types.Metric as TypesMetric


type alias Model =
    { metrics : RemoteData.WebData (List TypesMetric.Metric)
    , scenarioId1 : Int
    , scenarioId2 : Int
    }


model : Model
model =
    { metrics = RemoteData.NotAsked
    , scenarioId1 = 1
    , scenarioId2 = 1
    }
