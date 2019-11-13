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
            ( { model | scenarioDetail = RemoteData.Loading }, Cmd.none )

        Msg.HandleGetScenarioDetail (Ok scenarioDetail) ->
            ( { model | scenarioDetail = RemoteData.Success scenarioDetail }, Cmd.none )

        Msg.HandleGetScenarioDetail (Err error) ->
            ( { model | scenarioDetail = RemoteData.Failure error }, Cmd.none )
