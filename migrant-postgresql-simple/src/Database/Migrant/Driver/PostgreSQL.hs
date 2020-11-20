{-#LANGUAGE OverloadedStrings #-}
module Database.Migrant.Driver.PostgreSQL
where

import Database.Migrant.Driver.Class
import Database.Migrant.MigrationName
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Control.Monad (void)

instance Driver PostgreSQL.Connection where
  withTransaction action conn = PostgreSQL.withTransaction conn (action conn)

  initMigrations conn =
    void $
    PostgreSQL.execute_ conn
      "CREATE TABLE IF NOT EXISTS _migrations (id SERIAL PRIMARY KEY, name TEXT)"

  markUp name conn =
    void $
    PostgreSQL.execute conn
      "INSERT INTO _migrations (name) VALUES (?)"
      [unpackMigrationName name]

  markDown name conn =
    void $
    PostgreSQL.execute conn
      "DELETE FROM _migrations WHERE name = ?"
      [unpackMigrationName name]

  getMigrations conn = do
    result <- PostgreSQL.query_ conn
      "SELECT name FROM _migrations ORDER BY id"
    return [ MigrationName name | PostgreSQL.Only name <- result ]
