module Database.Migrant.MigrationName_Orphans
where

import Test.Tasty.QuickCheck
import Database.Migrant.MigrationName
import qualified Data.Text as Text

instance Arbitrary MigrationName where
  arbitrary = MigrationName . Text.pack <$> arbitrary
