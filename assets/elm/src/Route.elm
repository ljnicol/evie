module Route exposing (..)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as HtmlAttributes
import List
import Maybe
import Model
import String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


parser : Parser.Parser (Model.Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Model.ScenariosList (Parser.s "scenarios_list")
        , Parser.map Model.MultiScenarioComparison (Parser.s "multi_scenario_comparison" <?> scenarioParser)
        ]


scenarioParser : Query.Parser (List Int)
scenarioParser =
    Query.custom
        "scenarioId"
        (List.filterMap (\s -> String.toInt s))


routeToString : Model.Page -> String
routeToString page =
    case page of
        Model.ScenariosList ->
            "app#scenarios_list"

        Model.MultiScenarioComparison scenarioIds ->
            case scenarioIds of
                [] ->
                    "app#multi_scenario_comparison"

                _ ->
                    "app#multi_scenario_comparison?" ++ String.join "&" (List.map (\s -> "scenarioId=" ++ String.fromInt s) scenarioIds)


href : Model.Page -> Html.Attribute msg
href targetPage =
    HtmlAttributes.href (routeToString targetPage)


pushUrl : Navigation.Key -> Model.Page -> Cmd msg
pushUrl key route =
    Navigation.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Model.Page
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
