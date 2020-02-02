module View exposing (view)

import Browser
import Bulma.CDN as CDN
import Html exposing (Html, main_, text)
import Html.Attributes exposing (style)
import Model
import Msg exposing (Msg)
import Page.MultiScenarioComparison.View as MultiScenarioComparison
import Page.MultiScenarioMap.View as MultiScenarioMap
import Page.ScenariosList.View as ScenariosList
import Types.Page as TypesPage
import Views.Navbar as Navbar


view : Model.Model -> Browser.Document Msg
view model =
    { title = "Evie " ++ TypesPage.showPage model.page
    , body =
        [ main_ []
            [ Navbar.view model False
            , Html.div [ style "padding-top" "50px" ] [ viewPage model ]
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

        TypesPage.MultiScenarioMap _ _ ->
            Html.map Msg.MultiScenarioMap (MultiScenarioMap.view model.metrics)
