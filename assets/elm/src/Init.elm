module Init exposing (..)

import Browser.Navigation as Navigation
import Model
import Msg
import Update
import Url


type alias Flags =
    {}


init : Flags -> Url.Url -> Navigation.Key -> ( Model.Model, Cmd Msg.Msg )
init _ url key =
    let
        model =
            Model.model key

        ( newModel, urlCmd ) =
            Update.changeToUrl url model
    in
    ( newModel
    , Cmd.batch [  urlCmd ]
    )
