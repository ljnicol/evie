module Views.Navbar exposing (view)

import Bulma.Components as Components
import Bulma.Elements as Elements
import Bulma.Modifiers as Modifiers
import Html exposing (Html, text)
import Html.Attributes as HtmlAttributes
import Html.Events as Events
import Model
import Msg
import Route


myNavbarBurger : Bool -> Html Msg.Msg
myNavbarBurger isMenuOpen =
    Components.navbarBurger isMenuOpen
        []
        [ Html.span [] []
        , Html.span [] []
        , Html.span [] []
        ]


view : Model.Model -> Bool -> Html Msg.Msg
view model isMenuOpen =
    Components.fixedNavbar Modifiers.Top
        { color = Modifiers.Dark, transparent = False }
        []
        [ Components.navbarMenu isMenuOpen
            []
            [ Components.navbarStart []
                [ Components.navbarItemLink False [ Route.href Model.ScenariosList ] [ text "Scenarios" ]
                , Components.navbarItemLink False [ Route.href <| Model.MultiScenarioComparison [ 1, 2 ] ] [ text "Scenario Comparison" ]
                ]
            , Components.navbarEnd []
                [ Components.navbarItem False
                    []
                    []
                ]
            ]
        ]
