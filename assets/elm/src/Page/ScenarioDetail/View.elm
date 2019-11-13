module Page.ScenarioDetail.View exposing (view)

import Html exposing (Html, a, button, div, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.ScenarioDetail.Model as Model
import Page.ScenarioDetail.Msg as Msg
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
                        [ text "Not yet implemented"
                        ]
                    ]
                ]
            ]
        ]
