module Model exposing (..)

import Browser.Navigation as Navigation
import Page.MultiScenarioComparison.Model as MultiScenarioComparisonModel
import Page.ScenariosList.Model as ScenariosListModel
import Types.Page as TypesPage


model : Navigation.Key -> Model
model key =
    { page = TypesPage.ScenariosList
    , scenariosList = ScenariosListModel.model
    , scenario = MultiScenarioComparisonModel.model
    , key = key
    }


type alias Model =
    { page : TypesPage.Page
    , scenariosList : ScenariosListModel.Model
    , scenario : MultiScenarioComparisonModel.Model
    , key : Navigation.Key
    }
