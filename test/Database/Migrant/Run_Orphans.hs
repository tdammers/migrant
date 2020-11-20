{-#LANGUAGE LambdaCase #-}
module Database.Migrant.Run_Orphans
where

import Test.Tasty.QuickCheck
import Database.Migrant.Run
import qualified Data.Text as Text

instance Arbitrary MigrationDirection where
  arbitrary = arbitrary >>= \case
    True -> return MigrateUp
    False -> return MigrateDown
