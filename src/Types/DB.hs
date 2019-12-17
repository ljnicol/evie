module Types.DB where

import qualified Data.Aeson as Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Pool as Pool
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Database.SQLite.Simple.FromField as SQLiteSimple
import qualified Database.SQLite.Simple.Internal as SQSimple

data DatabaseEngine a = PostgreSQL (Pool.Pool PGSimple.Connection) | SQLite3 (Pool.Pool SQLiteSimple.Connection)

fromJSONField :: (Aeson.FromJSON a, Typeable a) => SQLiteSimple.FieldParser a
fromJSONField f@(SQSimple.Field (SQLiteSimple.SQLText blb) _) = do
  case (Aeson.eitherDecode . toLazyByteString . encodeUtf8Builder) blb of
    Left e ->
      SQLiteSimple.returnError SQLiteSimple.ConversionFailed f $
        "JSON decoding error: " ++ e
    Right x -> pure x
fromJSONField f =
  SQLiteSimple.returnError SQLiteSimple.ConversionFailed f $
    "expecting SQLBlob column type"
