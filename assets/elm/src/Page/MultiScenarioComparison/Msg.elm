module Page.MultiScenarioComparison.Msg exposing (..)

import Http
import Table as SortableTable
import Types.Scenario as TypesScenario


type Msg
    = LoadMultiScenarioComparison String (List Int)
    | HandleGetMultiScenarioComparison (Result Http.Error (List TypesScenario.MultiScenarioComparison))
    | NewTableState SortableTable.State
