module Page.ScenariosList.Update exposing (..)

import Browser.Navigation as Navigation
import Page.ScenariosList.Api as Api
import Page.ScenariosList.Model as Model
import Page.ScenariosList.Msg as Msg
import RemoteData


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.LoadScenariosList ->
            ( { model | scenariosList = RemoteData.Loading }, Api.getScenariosList )

        Msg.OpenMultiScenarioComparison scenario ->
            let
                year =
                    String.fromInt <| Maybe.withDefault 2019 <| List.head scenario.scenario_years
            in
            ( model, Navigation.load ("/app/scenario_detail/" ++ String.fromInt scenario.scenario_id ++ "/" ++ year) )

        --++ String.fromInt scenario.scenario_id) )
        Msg.HandleGetScenariosList (Ok reports) ->
            ( { model | scenariosList = RemoteData.Success reports }, Cmd.none )

        Msg.HandleGetScenariosList (Err error) ->
            ( { model | scenariosList = RemoteData.Failure error }, Cmd.none )
