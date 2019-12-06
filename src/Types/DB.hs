module Types.DB where

import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.SQLite.Simple as SQLiteSimple

data DatabaseEngine a = PostgreSQL (Pool.Pool PGSimple.Connection) | SQLite3 (Pool.Pool SQLiteSimple.Connection)

newtype VectorTile = VectorTile BS.ByteString

instance SQLiteSimple.FromRow VectorTile where
  fromRow =
    VectorTile
      <$> SQLiteSimple.field
