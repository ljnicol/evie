module Page.ScenariosList.Types exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


type alias Scenario =
    { scenario_id : Int
    , scenario_name : String
    , scenario_description : String
    , scenario_assumptions : String
    , scenario_spatial_table : String
    , scenario_years : List Int
    }


scenarioDecoder : Decode.Decoder Scenario
scenarioDecoder =
    Decode.succeed Scenario
        |> DecodePipeline.required "scenario_id" Decode.int
        |> DecodePipeline.required "scenario_name" Decode.string
        |> DecodePipeline.required "scenario_description" Decode.string
        |> DecodePipeline.required "scenario_assumptions" Decode.string
        |> DecodePipeline.required "scenario_spatial_table" Decode.string
        |> DecodePipeline.required "scenario_years" (Decode.list Decode.int)


decodeListScenario : Decode.Decoder (List Scenario)
decodeListScenario =
    Decode.list scenarioDecoder
