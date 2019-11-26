module View exposing (view)

import Browser
import Bulma.CDN as CDN
import Html exposing (Html, main_, text)
import Model
import Msg exposing (Msg)
import Page.MultiScenarioComparison.View as MultiScenarioComparison
import Page.ScenariosList.View as ScenariosList
import Types.Page as TypesPage
import Views.Navbar as Navbar


view : Model.Model -> Browser.Document Msg
view model =
    { title = "Evie"
    , body =
        [ main_ []
            [ --             Navbar.view model False,
              viewPage model
            ]
        ]
    }


viewPage : Model.Model -> Html Msg
viewPage model =
    case model.page of
        TypesPage.ScenariosList ->
            Html.map Msg.ScenariosList (ScenariosList.view model.scenariosList)

        TypesPage.MultiScenarioComparison _ _ ->
            Html.map Msg.MultiScenarioComparison (MultiScenarioComparison.view model.scenario)
