module Page.ScenarioDetail.Update exposing (..)

import Browser.Navigation as Navigation
import Page.ScenarioDetail.Api as Api
import Page.ScenarioDetail.Model as Model
import Page.ScenarioDetail.Msg as Msg
import RemoteData


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.LoadScenarioDetail ->
            ( { model | reportsScenarioDetail = RemoteData.Loading }, Api.getScenarioDetail )

        Msg.OpenReport report ->
            ( model, Navigation.load ("/app/search?lotAndPlan=" ++ report.scenario_id) )

        Msg.HandleGetScenarioDetail (Ok reports) ->
            ( { model | reportsScenarioDetail = RemoteData.Success reports }, Cmd.none )

        Msg.HandleGetScenarioDetail (Err error) ->
            ( { model | reportsScenarioDetail = RemoteData.Failure error }, Cmd.none )
