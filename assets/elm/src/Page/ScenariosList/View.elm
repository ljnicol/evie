module Page.ScenariosList.View exposing (view)

import Html exposing (Html, a, button, div, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.ScenariosList.Model as Model
import Page.ScenariosList.Msg as Msg
import RemoteData
import Types.Scenario as TypesScenario


view : Model.Model -> Html Msg.Msg
view model =
    section
        [ class "hero is-dark is-fullheight-with-navbar is-fullheight"
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
            [ table [ class "table" ]
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
                            []
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
    tr []
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
        , td []
            [ button
                [ class "button is-link", onClick (Msg.OpenScenarioDetail scenario) ]
                [ text "View" ]
            ]
        ]
