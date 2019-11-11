module Model exposing (..)

import Browser.Navigation as Navigation
import Page.ScenariosList.Model as ScenariosListModel
import Page.ScenarioDetail.Model as ScenarioDetailModel


model : Navigation.Key -> Model
model key =
    { page = ScenariosList
    , scenariosList = ScenariosListModel.model
    , scenario = ScenarioDetailModel.model
    , key = key
    }


type alias Model =
    { page : Page
    , scenariosList : ScenariosListModel.Model
    , scenario : ScenarioDetailModel.Model
    , key : Navigation.Key
    }


type Page
    = ScenariosList
    | ScenarioDetail
