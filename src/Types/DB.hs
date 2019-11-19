module Types.DB where

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.SQLite.Simple as SQLiteSimple

data DatabaseEngine a = PostgreSQL (Pool.Pool PGSimple.Connection) | SQLite3 (Pool.Pool SQLiteSimple.Connection)
