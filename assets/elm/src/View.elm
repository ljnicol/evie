module View exposing (view)

import Browser
import Bulma.CDN as CDN
import Html exposing (Html, main_, text)
import Model
import Msg exposing (Msg)
import Page.ScenarioDetail.View as ScenarioDetail
import Page.ScenariosList.View as ScenariosList
import Views.Navbar as Navbar


view : Model.Model -> Browser.Document Msg
view model =
    { title = "Evie"
    , body =
        [ main_ []
            [ CDN.stylesheet
              , Navbar.view model False
              , viewPage model
            ]
        ]
    }


viewPage : Model.Model -> Html Msg
viewPage model =
    case model.page of
        Model.ScenariosList ->
            Html.map Msg.ScenariosList (ScenariosList.view model.scenariosList)

        Model.ScenarioDetail ->
            Html.map Msg.ScenarioDetail (ScenarioDetail.view model.scenario)
