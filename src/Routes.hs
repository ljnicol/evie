module Routes where

import qualified Data.Proxy as Proxy
import Servant
--
import qualified Types.Scenario as ScenarioTypes

api :: Proxy.Proxy API
api = Proxy.Proxy

type API =
  "api"
    :> ( "scenarios"
           :> Get '[JSON] [ScenarioTypes.Scenario]
             :<|> "metrics"
           :> Capture "scenario_id" Int
           :> Capture "year" Int
           :> Get '[JSON] [ScenarioTypes.MetricData]
       )
    :<|> Raw
