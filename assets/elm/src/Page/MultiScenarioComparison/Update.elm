module Page.MultiScenarioComparison.Update exposing (..)

import Browser.Navigation as Navigation
import Page.MultiScenarioComparison.Api as Api
import Page.MultiScenarioComparison.Model as Model
import Page.MultiScenarioComparison.Msg as Msg
import RemoteData
import Types.Scenario as ScenarioTypes


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.LoadMultiScenarioComparison ->
            ( { model | multiScenarioComparison = RemoteData.Loading }, Cmd.none )

        Msg.HandleGetMultiScenarioComparison (Ok a) ->
            ( { model | multiScenarioComparison = RemoteData.Success a }, Cmd.none )

        Msg.HandleGetMultiScenarioComparison (Err error) ->
            ( { model | multiScenarioComparison = RemoteData.Failure error }, Cmd.none )
