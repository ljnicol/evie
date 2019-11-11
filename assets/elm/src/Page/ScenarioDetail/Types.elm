module Page.ScenarioDetail.Types exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


type alias ReportScenarioDetail =
    { scenario_id : String
    }


reportScenarioDetailDecoder : Decode.Decoder ReportScenarioDetail
reportScenarioDetailDecoder =
    Decode.succeed ReportScenarioDetail
        |> DecodePipeline.required "scenario_id" Decode.string


decodeScenarioDetailScenarioDetail : Decode.Decoder (List ReportScenarioDetail)
decodeScenarioDetailScenarioDetail =
    Decode.list reportScenarioDetailDecoder
