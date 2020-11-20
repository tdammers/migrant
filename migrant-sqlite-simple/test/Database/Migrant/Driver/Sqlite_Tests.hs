{-#LANGUAGE OverloadedStrings #-}
module Database.Migrant.Driver.Sqlite_Tests
( tests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Text (Text)

import Database.Migrant
import Database.Migrant.Driver.Sqlite
import qualified Database.SQLite.Simple as Sqlite

tests :: TestTree
tests = testGroup "SQLite"
  [ testCase "Setup/init" testSqliteSetup
  , testCase "Up" testSqliteUp
  , testCase "Down" testSqliteDown
  ]

testSqliteSetup :: Assertion
testSqliteSetup = do
  conn <- Sqlite.open ":memory:"
  migrate [] undefined undefined conn
  actual <- Sqlite.query_ conn "SELECT name FROM _migrations ORDER BY id"
  let expected = [] :: [Sqlite.Only Text]
  assertEqual "Migration table exists" expected actual

testSqliteUp :: Assertion
testSqliteUp = do
  let up "foo" conn =
        Sqlite.execute_ conn "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
      up name _ = error ("Invalid name " <> show name)
      down "foo" conn =
        Sqlite.execute_ conn "DROP TABLE foo"
      down name _ = error ("Invalid name " <> show name)
  conn <- Sqlite.open ":memory:"
  migrate ["foo"] up down conn

  actual <- Sqlite.query_ conn "SELECT name FROM _migrations ORDER BY id"
  let expected = [Sqlite.Only ("foo" :: Text)]
  assertEqual "Up migration logged" expected actual

  actual <- Sqlite.query_ conn "SELECT COUNT(1) FROM sqlite_master WHERE type = 'table' AND name = 'foo'"
  let expected = [Sqlite.Only (1 :: Int)]
  assertEqual "Up migration happened" expected actual

testSqliteDown :: Assertion
testSqliteDown = do
  let up "foo" conn =
        Sqlite.execute_ conn "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
      up name _ = error ("Invalid name " <> show name)
      down "foo" conn =
        Sqlite.execute_ conn "DROP TABLE foo"
      down name _ = error ("Invalid name " <> show name)
  conn <- Sqlite.open ":memory:"
  migrate ["foo"] up down conn
  migrate [] up down conn

  actual <- Sqlite.query_ conn "SELECT name FROM _migrations ORDER BY id"
  let expected = [] :: [Sqlite.Only Text]
  assertEqual "Down migration logged" expected actual

  actual <- Sqlite.query_ conn "SELECT COUNT(1) FROM sqlite_master WHERE type = 'table' AND name = 'foo'"
  let expected = [Sqlite.Only (0 :: Int)]
  assertEqual "Down migration happened" expected actual
