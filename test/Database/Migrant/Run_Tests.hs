{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}

module Database.Migrant.Run_Tests
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (isSuffixOf, isPrefixOf)
import Data.IORef
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)

import Database.Migrant.Run
import Database.Migrant.Driver.Class
import Database.Migrant.MigrationName
import Database.Migrant.MigrationName_Orphans
import Database.Migrant.Run_Orphans

tests = testGroup "Database.Migrant.Run"
    [ testGroup "makePlan"
        makePlanTests
    , testGroup "executePlan"
        executePlanTests
    , testGroup "migrateTests"
        migrateTests
    ]

makePlanTests :: [TestTree]
makePlanTests =
  [ testProperty "same inputs, empty diff" propSameInputEmptyDiff
  , testProperty "appended becomes diff" propAppendIsDiff
  , testProperty "removed becomes diff" propRemoveIsDiff
  , testProperty "diff length never longer than inputs combined" propDiffNotLargerThanInputs
  , testProperty "removeBeforeAdd" propRemoveBeforeAdd
  ]

propSameInputEmptyDiff migs =
  makePlan migs migs == []

propAppendIsDiff base ext =
  makePlan (base ++ ext) base == [(MigrateUp, m) | m <- ext]

propRemoveIsDiff base ext =
  makePlan base (base ++ ext) == [(MigrateDown, m) | m <- ext]

propDiffNotLargerThanInputs target current =
  length (makePlan target current) <= length target + length current

propRemoveBeforeAdd ext1 ext2 =
  all not ((==) <$> ext1 <*> ext2) ==>
  makePlan ext1 ext2 ==
  [(MigrateDown, e) | e <- ext2] ++
  [(MigrateUp, e) | e <- ext1]

executePlanTests :: [TestTree]
executePlanTests =
  [ testProperty "updates only" propExecuteUpdatesOnly
  , testProperty "rollbacks only" propExecuteRollbacksOnly
  , testProperty "mixed updates/rollbacks" propExecuteMixed
  ]

propExecuteUpdatesOnly target =
  let expected = [("MARK MigrateUp " <> unpackMigrationName n) | n <- target]
      plan = [(MigrateUp, n) | n <- target]
      getMigration _ _ = return ()
      logged = runChat (executePlan plan getMigration getMigration) []
  in logged == expected

propExecuteRollbacksOnly target =
  let expected = [("MARK MigrateDown " <> unpackMigrationName n) | n <- target]
      plan = [(MigrateDown, n) | n <- target]
      getMigration _ _ = return ()
      logged = runChat (executePlan plan getMigration getMigration) []
  in logged == expected

propExecuteMixed plan =
  let expected = [("MARK " <> Text.pack (show d) <> " " <> unpackMigrationName n) | (d, n) <- plan]
      getMigration _ _ = return ()
      logged = runChat (executePlan plan getMigration getMigration) []
  in logged == expected

migrateTests :: [TestTree]
migrateTests =
  [ testCase "transactionally do nothing" testMigrateNothing
  , testCase "transactionally do something" testMigrateSomething
  , testCase "roll back on host failure" testMigrateError
  ]

testMigrateNothing :: Assertion
testMigrateNothing = do
  let target = []
      up name = logChatQuery ("MigrateUp " <> unpackMigrationName name)
      down name = logChatQuery ("MigrateDown " <> unpackMigrationName name)
  actual <- runChatIO (migrate target up down) [[]]
  let expected =
        [ "BEGIN TRANSACTION"
        , "INIT"
        , "COMMIT TRANSACTION"
        ]
  assertEqual "migration log" expected actual

testMigrateSomething :: Assertion
testMigrateSomething = do
  let target = ["foo", "bar"]
      situation = ["bar", "quux"]
      up name = logChatQuery ("MigrateUp " <> unpackMigrationName name)
      down name = logChatQuery ("MigrateDown " <> unpackMigrationName name)
  actual <- runChatIO (migrate target up down) (repeat situation)
  let expected =
        [ "BEGIN TRANSACTION"
        , "INIT"
        , "MigrateDown bar"
        , "MARK MigrateDown bar"
        , "MigrateDown quux"
        , "MARK MigrateDown quux"
        , "MigrateUp foo"
        , "MARK MigrateUp foo"
        , "MigrateUp bar"
        , "MARK MigrateUp bar"
        , "COMMIT TRANSACTION"
        ]
  assertEqual "migration log" expected actual

testMigrateError :: Assertion
testMigrateError = do
  let target = ["foo"]
      situation = ["bar"]
      up name driver = throwIO DummyError
      down name = logChatQuery ("MigrateDown " <> unpackMigrationName name)
  actual <- runChatIO (migrate target up down) (repeat situation)
  let expected =
        [ "BEGIN TRANSACTION"
        , "INIT"
        , "MigrateDown bar"
        , "MARK MigrateDown bar"
        , "ROLLBACK TRANSACTION"
        ]
  assertEqual "migration log" expected actual

-- | A mockup driver we can use to test the migration planner/executor
data ChatDriver =
  ChatDriver
    { queries :: IORef [Text]
    , responses :: IORef [[MigrationName]]
    }

logChatQuery :: Text -> ChatDriver -> IO ()
logChatQuery msg ChatDriver { queries = queries } = 
  modifyIORef queries (++ [msg])

runChatIO :: (ChatDriver -> IO ()) -> [[MigrationName]] -> IO [Text]
runChatIO action responses = do
  queriesRef <- newIORef []
  responsesRef <- newIORef responses
  action (ChatDriver queriesRef responsesRef)
  readIORef queriesRef

runChat :: (ChatDriver -> IO ()) -> [[MigrationName]] -> [Text]
runChat action responses = unsafePerformIO (runChatIO action responses)

instance Driver ChatDriver where
  withTransaction action driver = do
    logChatQuery "BEGIN TRANSACTION" driver
    catch
        (do
          result <- action driver
          logChatQuery "COMMIT TRANSACTION" driver
          return result
        )
        (\(e :: SomeException) -> do
          logChatQuery "ROLLBACK TRANSACTION" driver
          return undefined
        )

  initMigrations driver = do
    logChatQuery "INIT" driver

  markUp name driver = do
    logChatQuery ("MARK MigrateUp " <> unpackMigrationName name) driver

  markDown name driver = do
    logChatQuery ("MARK MigrateDown " <> unpackMigrationName name) driver

  getMigrations driver = do
    (r:rs) <- readIORef (responses driver)
    writeIORef (responses driver) rs
    return r

data DummyError = DummyError
  deriving (Show)

instance Exception DummyError where
