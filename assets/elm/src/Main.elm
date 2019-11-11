module Main exposing (main)

import Browser
import Init
import Model
import Msg
import Update
import View


main : Program Init.Flags Model.Model Msg.Msg
main =
    Browser.application
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = Msg.OnUrlRequest
        , onUrlChange = Msg.OnUrlChange
        }
