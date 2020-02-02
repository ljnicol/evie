module Page.MultiScenarioMap.View exposing (..)

import Html exposing (Html, a, button, div, p, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.MultiScenarioMap.Model as Model
import Page.MultiScenarioMap.Msg as Msg
import RemoteData
import Types.Metric as TypesMetric


view : Model.Model -> Html Msg.Msg
view model =
    case model.metrics of
        RemoteData.Success ms ->
            div [] (List.map (metricButton model) ms)

        _ ->
            div [] [ text "Something went wrong." ]


metricButton : Model.Model -> TypesMetric.Metric -> Html Msg.Msg
metricButton model metric =
    button [ class "button is-primary", onClick (Msg.ShowMultiScenarioMap metric.id) ] [ text metric.name ]
