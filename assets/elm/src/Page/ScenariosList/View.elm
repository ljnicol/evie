module Page.ScenariosList.View exposing (view)

import Html exposing (Html, a, br, button, div, input, section, table, tbody, td, text, th, thead, tr)
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
                        [ div [ class "level-item" ] [ multiScenarioComparisonButton model ]
                        ]
                    ]
                ]
            ]
        , section
            [ class "section" ]
            []
        ]


multiScenarioComparisonButton : Model.Model -> Html Msg.Msg
multiScenarioComparisonButton model =
    let
        noScenariosSelected =
            case model.scenariosList of
                RemoteData.Success scenarios ->
                    List.isEmpty <| List.filter (\s -> s.selected) scenarios

                _ ->
                    True
    in
    button [ class "button is-link", onClick Msg.ShowMultiScenario, disabled noScenariosSelected ] [ text "Multi-Scenario Comparison" ]


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
            [ text <| String.join "," scenario.scenario_years
            ]
        , td [ class "has-text-centered" ]
            [ input [ type_ "checkbox", onCheck (Msg.SelectScenario scenario), checked scenario.selected ] [] ]
        , td [ class "has-text-centered", onClick (Msg.OpenScenarioDetail scenario) ]
            [ button [ class "button is-small is-link" ] [ text "View" ]
            ]
        ]
