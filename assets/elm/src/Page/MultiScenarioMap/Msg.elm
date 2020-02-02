module Page.MultiScenarioMap.Msg exposing (..)

import Http
import Types.Metric as TypesMetric


type Msg
    = NoOp
    | LoadMultiScenarioMap Int Int
    | HandleGetMultiScenarioMetrics (Result Http.Error (List TypesMetric.Metric))
    | ShowMultiScenarioMap Int
