module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Database.Migrant.Driver.HDBC_Tests

main :: IO ()
main = defaultMain $
  testGroup "Migrant"
    [ Database.Migrant.Driver.HDBC_Tests.tests
    ]
