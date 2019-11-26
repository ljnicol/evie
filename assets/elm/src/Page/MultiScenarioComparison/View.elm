module Page.MultiScenarioComparison.View exposing (view)

import Dict
import Html exposing (Html, a, button, div, p, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Page.MultiScenarioComparison.Model as Model
import Page.MultiScenarioComparison.Msg as Msg
import RemoteData
import Route
import Table as SortableTable
import Tuple
import Types.Page as TypesPage
import Types.Scenario as TypesScenario


view : Model.Model -> Html Msg.Msg
view model =
    div []
        (content
            model
        )


content : Model.Model -> List (Html Msg.Msg)
content model =
    case model.multiScenarioComparison of
        RemoteData.Success scenarios ->
            [ section
                [ class "section"
                ]
                [ div [ class "columns" ]
                    [ div [ class "column is-full" ]
                        [ yearButtons model.scenarioIds scenarios
                        ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column is-full" ]
                        [ sortableTable model scenarios
                        ]
                    ]
                ]
            ]

        RemoteData.Failure _ ->
            [ section
                [ class "section"
                ]
                [ div [] [ text "Something went wrong" ] ]
            ]

        RemoteData.Loading ->
            [ section
                [ class "section"
                ]
                [ div [] [ text "Loading" ] ]
            ]

        RemoteData.NotAsked ->
            [ section
                [ class "section"
                ]
                [ div [] [ text "Loading" ] ]
            ]


yearButtons : List Int -> List TypesScenario.MultiScenarioComparison -> Html Msg.Msg
yearButtons scenarioIds scenarios =
    let
        isCurrent y =
            case scenarios of
                x :: xs ->
                    y == x.year

                _ ->
                    False
    in
    case scenarios of
        x :: _ ->
            div [ class "field has-addons" ]
                (List.map
                    (\y ->
                        p [ class "control" ]
                            [ a
                                [ classList [ ( "button", True ), ( "is-primary", isCurrent y ) ], Route.href <| TypesPage.MultiScenarioComparison y scenarioIds ]
                                [ text y ]
                            ]
                    )
                    x.scenario.scenario_years
                )

        _ ->
            div [] []


sortableTable : Model.Model -> List TypesScenario.MultiScenarioComparison -> Html Msg.Msg
sortableTable model scenarios =
    let
        metricAccessors =
            case scenarios of
                x :: _ ->
                    TypesScenario.toMetricAccessor x.metric_data

                _ ->
                    []

        -- this will be implemented later to get orange highlights on the values
        bestPerformance =
            Dict.empty
    in
    SortableTable.view (sortableTableConfig bestPerformance metricAccessors) model.tableState scenarios


sortableTableConfig : Dict.Dict String Float -> List TypesScenario.MetricAccessor -> SortableTable.Config TypesScenario.MultiScenarioComparison Msg.Msg
sortableTableConfig bestPerformance metricAccessors =
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
    SortableTable.customConfig
        { toId = fToId
        , toMsg = Msg.NewTableState
        , customizations = tableCustomizations
        , columns =
            [ SortableTable.stringColumn "Name" fToScenarioName
            ]
                ++ toCustomColumns bestPerformance metricAccessors
        }


tableCustomizations : SortableTable.Customizations data msg
tableCustomizations =
    let
        def =
            SortableTable.defaultCustomizations

        ta =
            [ class "table is-bordered is-striped is-fullwidth is-hoverable" ]
    in
    { def | tableAttrs = def.tableAttrs ++ ta }


toCustomColumns : Dict.Dict String Float -> List TypesScenario.MetricAccessor -> List (SortableTable.Column TypesScenario.MultiScenarioComparison Msg.Msg)
toCustomColumns bestPerformance metricAccessors =
    let
        vd metricId a =
            let
                metric_data =
                    a.metric_data

                metric_1 =
                    Maybe.withDefault (TypesScenario.MetricData 0 "Empty" "Empty" "Empty" 0 "Empty") <| Dict.get metricId metric_data

                value =
                    metric_1.metric_value

                highlight =
                    if Just value == Dict.get metricId bestPerformance then
                        [ style "color" "orange" ]

                    else
                        []
            in
            { attributes = [], children = [ div highlight [ text <| String.fromFloat value ] ] }

        sort metricId a =
            let
                metric_data =
                    a.metric_data

                metric_1 =
                    Maybe.withDefault (TypesScenario.MetricData 0 "Empty" "Empty" "Empty" 0 "Empty") <| Dict.get metricId metric_data
            in
            String.fromFloat metric_1.metric_value
    in
    List.map (\( k, v ) -> SortableTable.veryCustomColumn { name = v, viewData = vd k, sorter = SortableTable.increasingOrDecreasingBy (sort k) }) metricAccessors
