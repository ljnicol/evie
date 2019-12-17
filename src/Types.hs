module Types where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

encodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

type Year = Text.Text
