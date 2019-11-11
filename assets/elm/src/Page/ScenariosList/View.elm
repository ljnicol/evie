module Page.ScenariosList.View exposing (view)

import Html exposing (Html, a, button, div, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.ScenariosList.Model as Model
import Page.ScenariosList.Msg as Msg
import Page.ScenariosList.Types as Types
import RemoteData


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
        RemoteData.Success reports ->
            reportsTable reports

        RemoteData.Failure err ->
            div [] [ text "Something went wrong" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.NotAsked ->
            div [] [ text "Loading" ]


reportsTable : List Types.Scenario -> Html Msg.Msg
reportsTable reports =
    if List.length reports > 0 then
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
                    (List.indexedMap tableRow reports)
                ]
            ]

    else
        div [] [ text "You have no scenarios. Contact your modeller to provision some." ]


tableRow : Int -> Types.Scenario -> Html Msg.Msg
tableRow index scenario =
    tr []
        [ td []
            [ text (String.fromInt <| scenario.scenario_id) ]
        , td []
            [ a [ onClick (Msg.OpenScenario scenario) ]
                [ text scenario.scenario_name ]
            ]
        , td []
            [ a [ onClick (Msg.OpenScenario scenario) ]
                [ text scenario.scenario_description ]
            ]
        , td []
            [ a [ onClick (Msg.OpenScenario scenario) ]
                [ text scenario.scenario_assumptions ]
            ]
        , td []
            [ a [ onClick (Msg.OpenScenario scenario) ]
                [ text <| String.join "," <| List.map String.fromInt scenario.scenario_years ]
            ]
        , td []
            [ button
                [ class "button is-link", onClick (Msg.OpenScenario scenario) ]
                [ text "View" ]
            ]
        ]