{-#LANGUAGE OverloadedStrings #-}
module Database.Migrant.Driver.Sqlite
where

import Database.Migrant.Driver.Class
import Database.Migrant.MigrationName
import qualified Database.SQLite.Simple as Sqlite

instance Driver Sqlite.Connection where
  withTransaction action conn = Sqlite.withTransaction conn (action conn)

  initMigrations conn =
    Sqlite.execute_ conn
      "CREATE TABLE IF NOT EXISTS _migrations (id INTEGER PRIMARY KEY, name TEXT)"

  markUp name conn =
    Sqlite.execute conn
      "INSERT INTO _migrations (name) VALUES (?)"
      [unpackMigrationName name]

  markDown name conn =
    Sqlite.execute conn
      "DELETE FROM _migrations WHERE name = ?"
      [unpackMigrationName name]

  getMigrations conn = do
    result <- Sqlite.query_ conn
      "SELECT name FROM _migrations ORDER BY id"
    return [ MigrationName name | Sqlite.Only name <- result ]
