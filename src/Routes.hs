{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Routes where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import Network.HTTP.Media ((//), (/:))
import Servant
import qualified Types.Scenario as ScenarioTypes

api :: Proxy.Proxy API
api = Proxy.Proxy

type API =
  "api"
    :> ( "scenarios"
           :> Get '[JSON] [ScenarioTypes.Scenario]
           :<|> "metrics"
             :> Capture "scenario_id" Integer
             :> Get '[JSON] [ScenarioTypes.MetricData]
       )
    :<|> "app" :> ("metric_detail" :> Capture "scenario_id" Integer :> Get '[Html] Text.Text :<|> Raw)

-- HTML content type with mimeRender instance
data Html

instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Html Text.Text where
  mimeRender _ val = BS.pack $ Text.unpack val
