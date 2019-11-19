module Page.MultiScenarioComparison.Model exposing (..)

import RemoteData
import Table as SortableTable
import Types.Scenario as TypesScenario


type alias Model =
    { scenarioIds : List Int
    , multiScenarioComparison : RemoteData.WebData (List TypesScenario.MultiScenarioComparison)
    , tableState : SortableTable.State
    }


model : Model
model =
    { scenarioIds = []
    , multiScenarioComparison = RemoteData.NotAsked
    , tableState = SortableTable.initialSort "scenarioId"
    }
