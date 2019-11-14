module Model exposing (..)

import Browser.Navigation as Navigation
import Page.MultiScenarioComparison.Model as MultiScenarioComparisonModel
import Page.ScenariosList.Model as ScenariosListModel


model : Navigation.Key -> Model
model key =
    { page = ScenariosList
    , scenariosList = ScenariosListModel.model
    , scenario = MultiScenarioComparisonModel.model
    , key = key
    }


type alias Model =
    { page : Page
    , scenariosList : ScenariosListModel.Model
    , scenario : MultiScenarioComparisonModel.Model
    , key : Navigation.Key
    }


type Page
    = ScenariosList
    | MultiScenarioComparison (List Int)
