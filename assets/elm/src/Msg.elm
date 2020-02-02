module Msg exposing (..)

import Browser
import Page.MultiScenarioComparison.Msg as MultiScenarioComparisonMsg
import Page.MultiScenarioMap.Msg as MultiScenarioMapMsg
import Page.ScenariosList.Msg as ScenariosListMsg
import Url


type Msg
    = NoOp
    | ScenariosList ScenariosListMsg.Msg
    | MultiScenarioComparison MultiScenarioComparisonMsg.Msg
    | MultiScenarioMap MultiScenarioMapMsg.Msg
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
