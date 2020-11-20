{-#LANGUAGE TypeFamilies #-}
module Database.Migrant.Driver.Class
where

import Database.Migrant.MigrationName (MigrationName)
import Data.Text (Text)
import Text.Printf (printf)
import Control.Monad

-- | A migrations driver abstracts over the database to run over and the
-- associated migration mechanism.
class Driver d where
  --  | Migrations should be done transactionally, so we need to provide a
  --  way of wrapping things in a transaction. Implementations should make
  --  sure that whatever runs inside a transaction is actually transactional
  --  (i.e., atomic), including host exceptions.
  withTransaction :: (d -> IO a) -> d -> IO a

  -- | Initialize the migrations system on the backend. This should generally
  -- involve creating a table of ordered migration names, e.g.:
  -- @@@
  -- CREATE TABLE IF NOT EXISTS _migrations (id SERIAL PRIMARY KEY, name TEXT);
  -- @@@
  --
  -- **Note** that 'initMigrations' should be idempotent, that is, executing it
  -- when the migrations system has already been initialized should be a no-op.
  initMigrations :: d -> IO ()

  -- | Mark a migration as \"executed\". Typically, this will @INSERT@ a row
  -- into the migrations table.
  markUp :: MigrationName -> d -> IO ()

  -- | Mark a migration as \"not executed\" / \"rolled back\". Typically, this
  -- will @DELETE@ a row from the migrations table.
  markDown :: MigrationName -> d -> IO ()

  -- | Get the list of migrations applied to the backend, in the order they
  -- were applied.
  getMigrations :: d -> IO [MigrationName]

data DummyDriver = DummyDriver

instance Driver DummyDriver where
  withTransaction = id
  initMigrations = const mzero
  markUp migration _ =
    printf "UP: %s" migration
  markDown migration _ =
    printf "DOWN: %s" migration
  getMigrations _ = return []
