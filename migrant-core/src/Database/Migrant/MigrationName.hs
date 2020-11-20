{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE DerivingVia #-}
module Database.Migrant.MigrationName
where

import Data.Text
import Text.Printf
import Data.String (IsString)

newtype MigrationName =
  MigrationName { unpackMigrationName :: Text }
  deriving Eq via Text
  deriving Show via Text
  deriving PrintfArg via Text
  deriving IsString via Text
