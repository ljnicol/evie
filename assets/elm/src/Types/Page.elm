module Types.Page exposing (..)


type Page
    = ScenariosList
    | MultiScenarioComparison String (List Int)
    | MultiScenarioMap Int Int String


showPage : Page -> String
showPage p =
    case p of
        ScenariosList ->
            "Scenarios"

        MultiScenarioComparison _ _ ->
            "Scenario Comparison"

        MultiScenarioMap _ _ _ ->
            "Scenario Spatial Comparison"
