module Page.MultiScenarioMap.Msg exposing (..)

import Http
import Types.Metric as TypesMetric


type Msg
    = NoOp
    | LoadMultiScenarioMap Int Int String
    | HandleGetMultiScenarioMetrics (Result Http.Error (List TypesMetric.Metric))
    | ShowMultiScenarioMap Int
