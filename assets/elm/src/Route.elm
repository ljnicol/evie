module Route exposing (..)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as HtmlAttributes
import Model
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


parser : Parser.Parser (Model.Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Model.ScenariosList (Parser.s "scenariosList")
        , Parser.map Model.MultiScenarioComparison (Parser.s "MultiScenarioComparison")
        ]


routeToString : Model.Page -> String
routeToString page =
    let
        pieces =
            case page of
                Model.ScenariosList ->
                    [ "scenariosList" ]

                Model.MultiScenarioComparison ->
                    [ "MultiScenarioComparison" ]
    in
    "app#" ++ String.join "/" pieces


href : Model.Page -> Html.Attribute msg
href targetPage =
    HtmlAttributes.href (routeToString targetPage)


pushUrl : Navigation.Key -> Model.Page -> Cmd msg
pushUrl key route =
    Navigation.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Model.Page
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
