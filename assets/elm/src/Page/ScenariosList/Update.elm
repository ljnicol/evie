module Page.ScenariosList.Update exposing (..)

import Browser.Navigation as Navigation
import Page.ScenariosList.Api as Api
import Page.ScenariosList.Model as Model
import Page.ScenariosList.Msg as Msg
import RemoteData
import Route


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.LoadScenariosList ->
            ( { model | scenariosList = RemoteData.Loading }, Api.getScenariosList )

        Msg.OpenScenarioDetail y scenario ->
            let
                year =
                    Maybe.withDefault (Maybe.withDefault "2019" <| List.head scenario.scenario_years) y
            in
            ( model, Navigation.load ("/app/scenario_detail/" ++ String.fromInt scenario.scenario_id ++ "/" ++ year) )

        --++ String.fromInt scenario.scenario_id) )
        Msg.HandleGetScenariosList (Ok scenarios) ->
            ( { model | scenariosList = RemoteData.Success scenarios }, Cmd.none )

        Msg.HandleGetScenariosList (Err error) ->
            ( { model | scenariosList = RemoteData.Failure error }, Cmd.none )

        Msg.SelectScenario s set ->
            let
                newScenarios =
                    case model.scenariosList of
                        RemoteData.Success oldScenarios ->
                            List.map
                                (\os ->
                                    { os
                                        | selected =
                                            if os.scenario_id == s.scenario_id then
                                                set

                                            else
                                                os.selected
                                    }
                                )
                                oldScenarios
                                |> RemoteData.Success

                        _ ->
                            model.scenariosList
            in
            ( { model | scenariosList = newScenarios }, Cmd.none )

        Msg.ShowMultiScenarioTable ->
            let
                scenariosToCompare =
                    case model.scenariosList of
                        RemoteData.Success scenarios ->
                            List.filter (\s -> s.selected) scenarios

                        _ ->
                            []
            in
            case scenariosToCompare of
                x :: xs ->
                    let
                        year =
                            Maybe.withDefault "2019" <| List.head x.scenario_years
                    in
                    ( model, Navigation.load <| Route.multiScenarioComparisonUrl year (List.map .scenario_id scenariosToCompare) )

                _ ->
                    ( model, Cmd.none )

        Msg.ShowMultiScenarioDetail ->
            let
                scenariosToCompare =
                    case model.scenariosList of
                        RemoteData.Success scenarios ->
                            List.filter (\s -> s.selected) scenarios

                        _ ->
                            []
            in
            case scenariosToCompare of
                x :: xs ->
                    let
                        year =
                            Maybe.withDefault "2019" <| List.head x.scenario_years
                    in
                    ( model, Navigation.load <| Route.multiScenarioComparisonDetailUrl year (List.map .scenario_id scenariosToCompare) )

                _ ->
                    ( model, Cmd.none )
