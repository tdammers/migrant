{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Database.Migrant.Driver.HDBC_Tests
( tests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Text (Text)
import Control.Monad (void)
import System.Environment
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite
import System.Process (callProcess)
import Control.Exception (catch, bracket)
import System.Random

import Database.Migrant
import Database.Migrant.Driver.HDBC

tests :: TestTree
tests = testGroup "SQLite"
  [ testCase "Setup/init" testHDBCSetup
  , testCase "Up" testHDBCUp
  , testCase "Down" testHDBCDown
  ]

withTestDB :: (HDBC.ConnWrapper -> IO a) -> IO a
withTestDB action = do
  bracket
    (Sqlite.connectSqlite3 ":memory:")
    HDBC.disconnect
    (action . HDBC.ConnWrapper)

testHDBCSetup :: Assertion
testHDBCSetup = withTestDB $ \conn -> do
  migrate [] undefined undefined conn
  actual <- HDBC.quickQuery' conn "SELECT name FROM _migrations ORDER BY id" []
  let expected = []
  assertEqual "Migration table exists" expected actual

testHDBCUp :: Assertion
testHDBCUp = do
  let up "foo" conn =
        void $ HDBC.run conn "CREATE TABLE foo (id INTEGER)" []
      up name _ = error ("Invalid name " <> show name)
      down "foo" conn =
        void $ HDBC.run conn "DROP TABLE foo" []
      down name _ = error ("Invalid name " <> show name)
  withTestDB $ \conn -> do
    migrate ["foo"] up down conn

    actual <- HDBC.quickQuery conn "SELECT name FROM _migrations ORDER BY id" []
    let expected = [[HDBC.toSql @Text "foo"]]
    assertEqual "Up migration logged" expected actual

    actual <- HDBC.quickQuery conn "SELECT COUNT(1) FROM foo" []
    let expected = [[HDBC.toSql @Int 0]]
    assertEqual "Up migration happened" expected actual

testHDBCDown :: Assertion
testHDBCDown = do
  let up "foo" conn =
        void $ HDBC.run conn "CREATE TABLE foo (id INTEGER)" []
      up name _ = error ("Invalid name " <> show name)
      down "foo" conn =
        void $ HDBC.run conn "DROP TABLE foo" []
      down name _ = error ("Invalid name " <> show name)

  withTestDB $ \conn -> do
    migrate ["foo"] up down conn
    migrate [] up down conn

    actual <- HDBC.quickQuery conn "SELECT name FROM _migrations ORDER BY id" []
    let expected = []
    assertEqual "Down migration logged" expected actual

    actual <-
      (HDBC.quickQuery conn "SELECT COUNT(1) FROM foo" [])
      `HDBC.catchSql`
      (\_ -> return [[HDBC.toSql @Int 12345]])
    let expected = [[HDBC.toSql @Int 12345]]
    assertEqual "Down migration happened" expected actual
