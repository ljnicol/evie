module Page.MultiScenarioMap.Update exposing (..)

import Browser.Navigation as Navigation
import Page.MultiScenarioMap.Api as Api
import Page.MultiScenarioMap.Model as Model
import Page.MultiScenarioMap.Msg as Msg
import RemoteData
import Route


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.NoOp ->
            ( model, Cmd.none )

        Msg.LoadMultiScenarioMap scenarioId1 scenarioId2 year ->
            ( { model | metrics = RemoteData.Loading, scenarioId1 = scenarioId1, scenarioId2 = scenarioId2, year = year }, Api.getMultiScenarioMetrics scenarioId1 scenarioId2 )

        Msg.HandleGetMultiScenarioMetrics (Ok metrics) ->
            ( { model | metrics = RemoteData.Success metrics }, Cmd.none )

        Msg.HandleGetMultiScenarioMetrics (Err error) ->
            let
                _ =
                    Debug.log "metrics" error
            in
            ( { model | metrics = RemoteData.Failure error }, Cmd.none )

        Msg.ShowMultiScenarioMap metricId ->
            ( model, Navigation.load <| Route.multiScenarioComparisonMapUrl model.scenarioId1 model.scenarioId2 metricId model.year )
