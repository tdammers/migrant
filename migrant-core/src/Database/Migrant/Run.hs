module Database.Migrant.Run
( migrate
, unsafeMigrate
, executePlan
, plan
, makePlan
, MigrationDirection (..)
)
where

import Database.Migrant.Driver.Class
import Database.Migrant.MigrationName

import Control.Monad (forM_)

data MigrationDirection
  = MigrateUp
  | MigrateDown
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Create a migration plan based on the current situation on the database,
-- and the specified target.
plan :: Driver d
     => [MigrationName]
     -> d
     -> IO [(MigrationDirection, MigrationName)]
plan target driver = do
  current <- getMigrations driver
  return $ makePlan target current

-- | Make a plan from a previously loaded current situation and the specified
-- target.
makePlan :: [MigrationName]
            -- ^ target
         -> [MigrationName]
            -- ^ current
         -> [(MigrationDirection, MigrationName)]
makePlan [] []
  -- Situation 0: nothing left to do
  = []
makePlan [] xs
  -- Situation 1: no more "up" targets left, but more migrations exist, so
  -- we need to roll those back.
  = [(MigrateDown, n) | n <- reverse xs]
makePlan xs []
  -- Situation 2: only "up" targets left, run them.
  = [(MigrateUp, n) | n <- xs]
makePlan (t:ts) (c:cs)
  -- Situation 3: "up" targets exist, and we also have existing migrations
  -- left. The same migration exists on both ends, so we can just skip
  -- forward.
  | t == c
  = makePlan ts cs
  -- Situation 4: both "up" targets and existing migrations are present but the
  -- do not match, so we need to roll back existing migrations until a
  -- consistent situation is restored.
  | otherwise
  = (MigrateDown, c):makePlan (t:ts) cs

-- | Apply a migration plan to a database.
-- This should generally be done within the same transaction as loading the
-- current situation from the database and creating a migration plan. Running
-- this action outside of a transaction may leave the database and migration
-- tracking in an inconsistent state.
executePlan :: Driver d
            => [(MigrationDirection, MigrationName)]
            -> (MigrationName -> d -> IO ())
            -> (MigrationName -> d -> IO ())
            -> d
            -> IO ()
executePlan migrationPlan up down driver = do
  forM_ migrationPlan $ \(direction, name) -> do
    let (run, mark) = case direction of
          MigrateUp -> (up, markUp)
          MigrateDown -> (down, markDown)
    run name driver
    mark name driver

-- | Safely (transactionally) infer and execute a migration.
migrate :: Driver d
        => [MigrationName]
           -- ^ Target situation
        -> (MigrationName -> d -> IO ())
           -- ^ Factory for \'up\' migration actions
        -> (MigrationName -> d -> IO ())
           -- ^ Factory for \'down\' migration actions
        -> d
        -> IO ()
migrate target up down driver =
  withTransaction (unsafeMigrate target up down) driver

-- | Infer and execute a migration in a non-transactional fashion. This means
-- that migration failures may leave the database and migration tracking in
-- an inconsistent state, so you should never call this outside of a
-- transaction.
unsafeMigrate :: Driver d
              => [MigrationName]
                 -- ^ Target situation
              -> (MigrationName -> d -> IO ())
                 -- ^ Factory for \'up\' migration actions
              -> (MigrationName -> d -> IO ())
                 -- ^ Factory for \'down\' migration actions
              -> d
              -> IO ()
unsafeMigrate target up down driver = do
  initMigrations driver
  migrationPlan <- plan target driver
  executePlan migrationPlan up down driver
