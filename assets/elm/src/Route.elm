module Route exposing (..)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as HtmlAttributes
import List
import Maybe
import String
import Types.Page as TypesPage
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


parser : Parser.Parser (TypesPage.Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map TypesPage.ScenariosList (Parser.s "scenarios_list")
        , Parser.map TypesPage.MultiScenarioComparison (Parser.s "multi_scenario_comparison" </> Parser.string <?> scenarioParser)
        , Parser.map TypesPage.MultiScenarioMap (Parser.s "multi_scenario_map" </> Parser.int </> Parser.int)
        ]


scenarioParser : Query.Parser (List Int)
scenarioParser =
    Query.custom
        "scenarioId"
        (List.filterMap (\s -> String.toInt s))


routeToString : TypesPage.Page -> String
routeToString page =
    case page of
        TypesPage.ScenariosList ->
            "app#scenarios_list"

        TypesPage.MultiScenarioComparison year scenarioIds ->
            multiScenarioComparisonUrl year scenarioIds

        TypesPage.MultiScenarioMap scenarioId1 scenarioId2 ->
            multiScenarioComparisonMetricsUrl scenarioId1 scenarioId2


multiScenarioComparisonUrl : String -> List Int -> String
multiScenarioComparisonUrl year scenarioIds =
    case scenarioIds of
        [] ->
            "app#scenarios_list"

        _ ->
            "app#multi_scenario_comparison/" ++ year ++ "?" ++ String.join "&" (List.map (\s -> "scenarioId=" ++ String.fromInt s) scenarioIds)


multiScenarioComparisonDetailUrl : String -> List Int -> String
multiScenarioComparisonDetailUrl year scenarioIds =
    case scenarioIds of
        [] ->
            "app#scenarios_list"

        _ ->
            "app/scenario_comparison?year=" ++ year ++ "&" ++ String.join "&" (List.map (\s -> "scenarioId=" ++ String.fromInt s) scenarioIds)


multiScenarioComparisonMetricsUrl : Int -> Int -> String
multiScenarioComparisonMetricsUrl scenarioId1 scenarioId2 =
    "app#multi_scenario_map/" ++ String.fromInt scenarioId1 ++ "/" ++ String.fromInt scenarioId2


multiScenarioComparisonMapUrl : Int -> Int -> Int -> String
multiScenarioComparisonMapUrl scenarioId1 scenarioId2 metricId =
    "app/scenario_comparison_map/" ++ String.fromInt scenarioId1 ++ "/" ++ String.fromInt scenarioId2 ++ "/" ++ String.fromInt metricId


href : TypesPage.Page -> Html.Attribute msg
href targetPage =
    HtmlAttributes.href (routeToString targetPage)


pushUrl : Navigation.Key -> TypesPage.Page -> Cmd msg
pushUrl key route =
    Navigation.pushUrl key (routeToString route)


fromUrl : Url -> Maybe TypesPage.Page
fromUrl url =
    --    We have the path in the fragment to handle client-side routing.
    --    We need to split the current fragment on '?' and then assign each section to the path
    --    and the query as required.
    let
        oldFragmentSplit =
            Maybe.withDefault "" url.fragment |> String.split "?"

        ( newPath, newQuery ) =
            case oldFragmentSplit of
                path :: query :: _ ->
                    ( path, Just query )

                path :: [] ->
                    ( path, Nothing )

                _ ->
                    ( "", Nothing )
    in
    { url | path = newPath, fragment = Nothing, query = newQuery }
        |> Parser.parse parser
