module Page.MultiScenarioComparison.Update exposing (..)

import Page.MultiScenarioComparison.Api as Api
import Page.MultiScenarioComparison.Model as Model
import Page.MultiScenarioComparison.Msg as Msg
import RemoteData


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.LoadMultiScenarioComparison year scenarioIds ->
            ( { model | multiScenarioComparison = RemoteData.Loading, year = year, scenarioIds = scenarioIds }, Api.getMultiScenarioComparison year scenarioIds )

        Msg.HandleGetMultiScenarioComparison (Ok a) ->
            ( { model | multiScenarioComparison = RemoteData.Success a }, Cmd.none )

        Msg.HandleGetMultiScenarioComparison (Err error) ->
            ( { model | multiScenarioComparison = RemoteData.Failure error }, Cmd.none )

        Msg.NewTableState s ->
            ( { model | tableState = s }, Cmd.none )
