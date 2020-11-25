{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE UndecidableInstances #-}
module Database.Migrant.Driver.HDBC
where

import Database.Migrant.Driver.Class
import Database.Migrant.MigrationName
import qualified Database.HDBC as HDBC
import Control.Monad (void)

instance Driver HDBC.ConnWrapper where
  withTransaction action conn = HDBC.withTransaction conn action

  initMigrations conn =
    void $
    HDBC.runRaw conn q
    where
      q = case HDBC.proxiedClientName conn of
        "postgresql" ->
          "CREATE TABLE IF NOT EXISTS _migrations (id SERIAL PRIMARY KEY, name TEXT NOT NULL)"
        "sqlite3" ->
          "CREATE TABLE IF NOT EXISTS _migrations (id INTEGER NOT NULL PRIMARY KEY, name TEXT NOT NULL)"
        "mysql" ->
          "CREATE TABLE IF NOT EXISTS _migrations (id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(256) NOT NULL)"
        _ ->
          -- ANSI SQL 2003 standard syntax
          "CREATE TABLE IF NOT EXISTS _migrations (id INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY, name VARCHAR(256) NOT NULL)"

  markUp name conn =
    void $
    HDBC.run conn
      "INSERT INTO _migrations (name) VALUES (?)"
      [HDBC.toSql $ unpackMigrationName name]

  markDown name conn =
    void $
    HDBC.quickQuery conn
      "DELETE FROM _migrations WHERE name = ?"
      [HDBC.toSql $ unpackMigrationName name]

  getMigrations conn = do
    result <- HDBC.quickQuery conn
      "SELECT name FROM _migrations ORDER BY id"
      []
    return [ MigrationName . HDBC.fromSql $ name | [name] <- result ]
