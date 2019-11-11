module Page.ScenarioDetail.View exposing (view)

import Html exposing (Html, a, button, div, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.ScenarioDetail.Model as Model
import Page.ScenarioDetail.Msg as Msg
import Page.ScenarioDetail.Types as Types
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
    case model.reportsScenarioDetail of
        RemoteData.Success reports ->
            reportsTable reports

        RemoteData.Failure err ->
            div [] [ text "Something went wrong" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.NotAsked ->
            div [] [ text "Loading" ]


reportsTable : List Types.ReportScenarioDetail -> Html Msg.Msg
reportsTable reports =
    if List.length reports > 0 then
        div [ class "table-container" ]
            [ table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] []
                        , th []
                            [ text "Property ID"
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
        div [] [ text "No scenario." ]


tableRow : Int -> Types.ReportScenarioDetail -> Html Msg.Msg
tableRow index reportScenarioDetail =
    tr []
        [ td []
            [ text (String.fromInt <| index + 1) ]
        , td []
            [ a [ onClick (Msg.OpenReport reportScenarioDetail) ]
                [ text reportScenarioDetail.scenario_id ]
            ]
        , td []
            [ button
                [ class "button is-link", onClick (Msg.OpenReport reportScenarioDetail) ]
                [ text "View" ]
            ]
        ]
