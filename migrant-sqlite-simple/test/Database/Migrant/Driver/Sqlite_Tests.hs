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
  , testCase "Full rollback" testSqliteFullRollback
  , testCase "Partial rollack" testSqlitePartialRollback
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

  actual <- Sqlite.query_ conn "SELECT name FROM _migrations ORDER BY id DESC"
  let expected = [] :: [Sqlite.Only Text]
  assertEqual "Down migration logged" expected actual

  actual <- Sqlite.query_ conn "SELECT COUNT(1) FROM sqlite_master WHERE type = 'table' AND name = 'foo'"
  let expected = [Sqlite.Only (0 :: Int)]
  assertEqual "Down migration happened" expected actual

testSqliteFullRollback :: Assertion
testSqliteFullRollback = do
  let up "one" conn = do
        Sqlite.execute_ conn "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
      up "two" conn = do
        Sqlite.execute_ conn "INSERT INTO foo (id) VALUES (1)"
      up name _ = error ("Invalid name " <> show name)
      down "one" conn = do
        Sqlite.execute_ conn "DROP TABLE foo"
      down "two" conn =
        Sqlite.execute_ conn "DELETE FROM foo WHERE 1"  
      down name _ = error ("Invalid name " <> show name)
  conn <- Sqlite.open ":memory:"
  migrate ["one", "two"] up down conn
  migrate [] up down conn

  actual <- Sqlite.query_ conn "SELECT name FROM _migrations ORDER BY id DESC"
  let expected = [] :: [Sqlite.Only Text]
  assertEqual "Down migration logged" expected actual

  actual <- Sqlite.query_ conn "SELECT COUNT(1) FROM sqlite_master WHERE type = 'table' AND name = 'foo'"
  let expected = [Sqlite.Only (0 :: Int)]
  assertEqual "Down migration happened" expected actual

testSqlitePartialRollback :: Assertion
testSqlitePartialRollback = do
  let up "one" conn = do
        putStrLn "up one"
        Sqlite.execute_ conn "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
      up "two" conn = do
        putStrLn "up two"
        Sqlite.execute_ conn "CREATE TABLE boo (id INTEGER PRIMARY KEY)"
      up "three" conn = do
        putStrLn "up three"
        Sqlite.execute_ conn "INSERT INTO boo (id) VALUES (1)"
      up name _ = error ("Invalid name " <> show name)
      down "one" conn = do
        putStrLn "down one"
        Sqlite.execute_ conn "DROP TABLE foo"
      down "two" conn = do
        putStrLn "down two"
        Sqlite.execute_ conn "DROP TABLE boo"
      down "three" conn = do
        putStrLn "down three"
        Sqlite.execute_ conn "DELETE FROM boo WHERE 1"  
      down name _ = error ("Invalid name " <> show name)
  conn <- Sqlite.open ":memory:"
  migrate ["one", "two", "three"] up down conn
  migrate ["one"] up down conn

  actual <- Sqlite.query_ conn "SELECT name FROM _migrations ORDER BY id DESC"
  let expected = [Sqlite.Only "one"] :: [Sqlite.Only Text]
  assertEqual "Down migration logged" expected actual

  actual <- Sqlite.query_ conn "SELECT COUNT(1) FROM sqlite_master WHERE type = 'table' AND name = 'boo'"
  let expected = [Sqlite.Only (0 :: Int)]
  assertEqual "Down migration happened" expected actual


