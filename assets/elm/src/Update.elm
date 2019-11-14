module Update exposing (changeToUrl, update)

import Browser
import Browser.Navigation as Navigation
import Model
import Msg
import Page.MultiScenarioComparison.Msg as MultiScenarioComparisonMsg
import Page.MultiScenarioComparison.Update as MultiScenarioComparisonUpdate
import Page.ScenariosList.Msg as ScenariosListMsg
import Page.ScenariosList.Update as ScenariosListUpdate
import Route
import Url


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.NoOp ->
            ( model, Cmd.none )

        Msg.OnUrlChange url ->
            changeToUrl url model

        Msg.OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Navigation.pushUrl model.key (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        Msg.ScenariosList m ->
            ScenariosListUpdate.update m model.scenariosList
                |> updateWith (\s -> { model | scenariosList = s }) Msg.ScenariosList model

        Msg.MultiScenarioComparison m ->
            MultiScenarioComparisonUpdate.update m model.scenario
                |> updateWith (\s -> { model | scenario = s }) Msg.MultiScenarioComparison model


updateWith : (subModel -> Model.Model) -> (subMsg -> Msg.Msg) -> Model.Model -> ( subModel, Cmd subMsg ) -> ( Model.Model, Cmd Msg.Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


showScenariosList : Model.Model -> ( Model.Model, Cmd Msg.Msg )
showScenariosList model =
    let
        newModel =
            { model | page = Model.ScenariosList }
    in
    ScenariosListUpdate.update ScenariosListMsg.LoadScenariosList model.scenariosList
        |> updateWith (\s -> { newModel | scenariosList = s }) Msg.ScenariosList model


showMultiScenarioComparison : Model.Model -> List Int -> ( Model.Model, Cmd Msg.Msg )
showMultiScenarioComparison model scenarioIds =
    let
        newModel =
            { model | page = Model.MultiScenarioComparison scenarioIds }
    in
    MultiScenarioComparisonUpdate.update (MultiScenarioComparisonMsg.LoadMultiScenarioComparison scenarioIds) model.scenario
        |> updateWith (\s -> { newModel | scenario = s }) Msg.MultiScenarioComparison model


changeToUrl : Url.Url -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
changeToUrl url model =
    case Route.fromUrl url of
        Nothing ->
            ( model, Route.pushUrl model.key Model.ScenariosList )

        Just Model.ScenariosList ->
            showScenariosList model

        Just (Model.MultiScenarioComparison scenarioIds) ->
            showMultiScenarioComparison model scenarioIds
