module Page.ScenariosList.View exposing (view)

import Html exposing (Html, a, br, button, div, h1, input, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, disabled, type_)
import Html.Events exposing (onCheck, onClick)
import Page.ScenariosList.Model as Model
import Page.ScenariosList.Msg as Msg
import RemoteData
import Types.Scenario as TypesScenario


view : Model.Model -> Html Msg.Msg
view model =
    div []
        [ section
            [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-full" ]
                        [ dummyTable model
                        ]
                    ]
                ]
            , br [] []
            , div
                [ class "container" ]
                [ div [ class "level" ]
                    [ div [ class "level-left" ]
                        []
                    , div [ class "level-right" ]
                        [ div [ class "level-item" ]
                            [ h1 [ class "title is-5" ] [ text "Compare Scenarios:" ]
                            ]
                        , div [ class "level-item" ]
                            [ multiScenarioComparisonTable model
                            ]
                        , div [ class "level-item" ]
                            [ multiScenarioComparisonDetail model
                            ]
                        , div [ class "level-item" ]
                            [ multiScenarioComparisonMap model
                            ]
                        ]
                    ]
                ]
            ]
        , section
            [ class "section" ]
            []
        ]


multiScenarioComparisonMap : Model.Model -> Html Msg.Msg
multiScenarioComparisonMap model =
    let
        ( noScenariosSelected, moreThanTwoSelected ) =
            case model.scenariosList of
                RemoteData.Success scenarios ->
                    let
                        selected =
                            List.filter (\s -> s.selected) scenarios
                    in
                    ( List.isEmpty <| selected, List.length selected > 2 )

                _ ->
                    ( True, False )
    in
    button [ class "button is-primary", onClick Msg.ShowMultiScenarioMetrics, disabled ((||) noScenariosSelected moreThanTwoSelected) ] [ text "Map" ]


multiScenarioComparisonTable : Model.Model -> Html Msg.Msg
multiScenarioComparisonTable model =
    let
        noScenariosSelected =
            case model.scenariosList of
                RemoteData.Success scenarios ->
                    List.isEmpty <| List.filter (\s -> s.selected) scenarios

                _ ->
                    True
    in
    button [ class "button is-primary", onClick Msg.ShowMultiScenarioTable, disabled noScenariosSelected ] [ text "Table" ]


multiScenarioComparisonDetail : Model.Model -> Html Msg.Msg
multiScenarioComparisonDetail model =
    let
        noScenariosSelected =
            case model.scenariosList of
                RemoteData.Success scenarios ->
                    List.isEmpty <| List.filter (\s -> s.selected) scenarios

                _ ->
                    True
    in
    button [ class "button is-primary", onClick Msg.ShowMultiScenarioDetail, disabled noScenariosSelected ] [ text "Scenario Detail" ]


dummyTable : Model.Model -> Html Msg.Msg
dummyTable model =
    case model.scenariosList of
        RemoteData.Success scenarios ->
            reportsTable scenarios

        RemoteData.Failure err ->
            div [] [ text "Something went wrong" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.NotAsked ->
            div [] [ text "Loading" ]


reportsTable : List TypesScenario.Scenario -> Html Msg.Msg
reportsTable scenarios =
    if List.length scenarios > 0 then
        div [ class "table-container" ]
            [ table [ class "table is-bordered is-striped is-fullwidth is-hoverable" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "ID" ]
                        , th []
                            [ text "Name"
                            ]
                        , th []
                            [ text "Description"
                            ]
                        , th []
                            [ text "Assumptions"
                            ]
                        , th []
                            [ text "Years"
                            ]
                        , th []
                            [ text "Select" ]
                        , th []
                            [ text "View" ]
                        ]
                    ]
                , tbody []
                    (List.indexedMap tableRow scenarios)
                ]
            ]

    else
        div [] [ text "You have no scenarios. Contact your modeller to provision some." ]


tableRow : Int -> TypesScenario.Scenario -> Html Msg.Msg
tableRow index scenario =
    tr [ class "is-vcentered" ]
        [ td []
            [ text (String.fromInt <| scenario.scenario_id) ]
        , td []
            [ text scenario.scenario_name
            ]
        , td []
            [ text scenario.scenario_description
            ]
        , td []
            [ text scenario.scenario_assumptions
            ]
        , td []
            (List.map (\y -> button [ class "button is-small", onClick (Msg.OpenScenarioDetail (Just y) scenario) ] [ text y ]) scenario.scenario_years)
        , td [ class "has-text-centered" ]
            [ input [ type_ "checkbox", onCheck (Msg.SelectScenario scenario), checked scenario.selected ] [] ]
        , td [ class "has-text-centered", onClick (Msg.OpenScenarioDetail Nothing scenario) ]
            [ button [ class "button is-small is-primary" ] [ text "View" ]
            ]
        ]
