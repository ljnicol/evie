module Msg exposing (..)

import Browser
import Page.ScenarioDetail.Msg as ScenarioDetailMsg
import Page.ScenariosList.Msg as ScenariosListMsg
import Url


type Msg
    = NoOp
    | ScenariosList ScenariosListMsg.Msg
    | ScenarioDetail ScenarioDetailMsg.Msg
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
