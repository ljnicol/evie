module Update exposing (changeToUrl, update)

import Browser
import Browser.Navigation as Navigation
import Model
import Msg
import Page.MultiScenarioComparison.Msg as MultiScenarioComparisonMsg
import Page.MultiScenarioComparison.Update as MultiScenarioComparisonUpdate
import Page.MultiScenarioMap.Msg as MultiScenarioMapMsg
import Page.MultiScenarioMap.Update as MultiScenarioMapUpdate
import Page.ScenariosList.Msg as ScenariosListMsg
import Page.ScenariosList.Update as ScenariosListUpdate
import Route
import Types.Page as TypesPage
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

        Msg.MultiScenarioMap m ->
            MultiScenarioMapUpdate.update m model.metrics
                |> updateWith (\me -> { model | metrics = me }) Msg.MultiScenarioMap model


updateWith : (subModel -> Model.Model) -> (subMsg -> Msg.Msg) -> Model.Model -> ( subModel, Cmd subMsg ) -> ( Model.Model, Cmd Msg.Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


showScenariosList : Model.Model -> ( Model.Model, Cmd Msg.Msg )
showScenariosList model =
    let
        newModel =
            { model | page = TypesPage.ScenariosList }
    in
    ScenariosListUpdate.update ScenariosListMsg.LoadScenariosList model.scenariosList
        |> updateWith (\s -> { newModel | scenariosList = s }) Msg.ScenariosList model


showMultiScenarioComparison : Model.Model -> String -> List Int -> ( Model.Model, Cmd Msg.Msg )
showMultiScenarioComparison model year scenarioIds =
    let
        newModel =
            { model | page = TypesPage.MultiScenarioComparison year scenarioIds }
    in
    MultiScenarioComparisonUpdate.update (MultiScenarioComparisonMsg.LoadMultiScenarioComparison year scenarioIds) model.scenario
        |> updateWith (\s -> { newModel | scenario = s }) Msg.MultiScenarioComparison model


showMultiScenarioMap : Model.Model -> Int -> Int -> ( Model.Model, Cmd Msg.Msg )
showMultiScenarioMap model scenarioId1 scenarioId2 =
    let
        newModel =
            { model | page = TypesPage.MultiScenarioMap scenarioId1 scenarioId2 }
    in
    MultiScenarioMapUpdate.update (MultiScenarioMapMsg.LoadMultiScenarioMap scenarioId1 scenarioId2) model.metrics
        |> updateWith (\s -> { newModel | metrics = s }) Msg.MultiScenarioMap model


changeToUrl : Url.Url -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
changeToUrl url model =
    case Route.fromUrl url of
        Nothing ->
            ( model, Route.pushUrl model.key TypesPage.ScenariosList )

        Just TypesPage.ScenariosList ->
            showScenariosList model

        Just (TypesPage.MultiScenarioComparison year scenarioIds) ->
            showMultiScenarioComparison model year scenarioIds

        Just (TypesPage.MultiScenarioMap scenarioId1 scenarioId2) ->
            showMultiScenarioMap model scenarioId1 scenarioId2
