{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Database.Migrant.Driver.PostgreSQL_Tests
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
import qualified Database.PostgreSQL.Simple as PostgreSQL
import System.Process (callProcess)
import Control.Exception (catch, bracket)
import System.Random

import Database.Migrant
import Database.Migrant.Driver.PostgreSQL

tests :: TestTree
tests = testGroup "PostgreSQL"
  [ testCase "Setup/init" testPostgreSQLSetup
  , testCase "Up" testPostgreSQLUp
  , testCase "Down" testPostgreSQLDown
  ]

withTestDB :: (PostgreSQL.Connection -> IO a) -> IO a
withTestDB action = do
  i <- randomRIO (1000000, 9999999)
  let dbname = "_migrant_test_" <> show (i :: Int)
  callProcess "createdb" [dbname]
  let dsn = "dbname=" <> dbname
  bracket
    (PostgreSQL.connectPostgreSQL $ Text.encodeUtf8 . Text.pack $ dsn)
    (\conn -> do
        PostgreSQL.close conn
        callProcess "dropdb" [dbname] `catch` (\(e :: IOError) -> return ())
    )
    action

testPostgreSQLSetup :: Assertion
testPostgreSQLSetup = withTestDB $ \conn -> do
  migrate [] undefined undefined conn
  actual <- PostgreSQL.query_ conn "SELECT name FROM _migrations ORDER BY id"
  let expected = [] :: [PostgreSQL.Only Text]
  assertEqual "Migration table exists" expected actual

testPostgreSQLUp :: Assertion
testPostgreSQLUp = do
  let up "foo" conn =
        void $ PostgreSQL.execute_ conn "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
      up name _ = error ("Invalid name " <> show name)
      down "foo" conn =
        void $ PostgreSQL.execute_ conn "DROP TABLE foo"
      down name _ = error ("Invalid name " <> show name)
  withTestDB $ \conn -> do
    migrate ["foo"] up down conn

    actual <- PostgreSQL.query_ conn "SELECT name FROM _migrations ORDER BY id"
    let expected = [PostgreSQL.Only ("foo" :: Text)]
    assertEqual "Up migration logged" expected actual

    actual <- PostgreSQL.query_ conn "SELECT COUNT(1) FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'foo'"
    let expected = [PostgreSQL.Only (1 :: Int)]
    assertEqual "Up migration happened" expected actual

testPostgreSQLDown :: Assertion
testPostgreSQLDown = do
  let up "foo" conn =
        void $ PostgreSQL.execute_ conn "CREATE TABLE foo (id INTEGER PRIMARY KEY)"
      up name _ = error ("Invalid name " <> show name)
      down "foo" conn =
        void $ PostgreSQL.execute_ conn "DROP TABLE foo"
      down name _ = error ("Invalid name " <> show name)

  withTestDB $ \conn -> do
    migrate ["foo"] up down conn
    migrate [] up down conn

    actual <- PostgreSQL.query_ conn "SELECT name FROM _migrations ORDER BY id"
    let expected = [] :: [PostgreSQL.Only Text]
    assertEqual "Down migration logged" expected actual

    actual <- PostgreSQL.query_ conn "SELECT COUNT(1) FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'foo'"
    let expected = [PostgreSQL.Only (0 :: Int)]
    assertEqual "Down migration happened" expected actual
