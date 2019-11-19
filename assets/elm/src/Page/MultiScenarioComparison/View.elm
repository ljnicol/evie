module Page.MultiScenarioComparison.View exposing (view)

import Dict
import Html exposing (Html, a, button, div, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.MultiScenarioComparison.Model as Model
import Page.MultiScenarioComparison.Msg as Msg
import RemoteData
import Table as SortableTable
import Tuple
import Types.Scenario as TypesScenario


view : Model.Model -> Html Msg.Msg
view model =
    section
        [ class "hero is-fullheight-with-navbar is-fullheight"
        ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ dummyTable model
                        ]
                    ]
                ]
            ]
        ]


dummyTable : Model.Model -> Html Msg.Msg
dummyTable model =
    case model.multiScenarioComparison of
        RemoteData.Success scenarios ->
            sortableTable model scenarios

        RemoteData.Failure err ->
            let
                _ =
                    Debug.log "Remote data failure" err
            in
            div [] [ text "Something went wrong" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.NotAsked ->
            div [] [ text "Loading" ]


sortableTable : Model.Model -> List TypesScenario.MultiScenarioComparison -> Html Msg.Msg
sortableTable model scenarios =
    let
        metricAccessors =
            case scenarios of
                x :: _ ->
                    TypesScenario.toMetricAccessor x.metric_data

                _ ->
                    []
    in
    SortableTable.view (sortableTableConfig metricAccessors) model.tableState scenarios


sortableTableConfig : List TypesScenario.MetricAccessor -> SortableTable.Config TypesScenario.MultiScenarioComparison Msg.Msg
sortableTableConfig metricAccessors =
    let
        fToId a =
            let
                scenario =
                    a.scenario
            in
            String.fromInt scenario.scenario_id

        fToScenarioName a =
            let
                scenario =
                    a.scenario
            in
            scenario.scenario_name

        fToScenarioMetricData a =
            a.metric_data
    in
    SortableTable.config
        { toId = fToId
        , toMsg = Msg.NewTableState
        , columns =
            [ SortableTable.stringColumn "Name" fToScenarioName
            ]
                ++ toCustomColumns metricAccessors
        }


toCustomColumns : List TypesScenario.MetricAccessor -> List (SortableTable.Column TypesScenario.MultiScenarioComparison Msg.Msg)
toCustomColumns metricAccessors =
    let
        vd metricId a =
            let
                metric_data =
                    a.metric_data

                metric_1 =
                    Maybe.withDefault (TypesScenario.MetricData 0 "Empty" "Empty" "Empty" 0 "Empty") <| Dict.get metricId metric_data
            in
            String.fromFloat metric_1.metric_value
    in
    List.map (\( k, v ) -> SortableTable.customColumn { name = v, viewData = vd k, sorter = SortableTable.increasingOrDecreasingBy (vd k) }) metricAccessors
