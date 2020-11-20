module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (isSuffixOf, isPrefixOf)

import qualified Database.Migrant.Run_Tests

main :: IO ()
main = defaultMain $
  testGroup "Migrant"
    [ Database.Migrant.Run_Tests.tests
    ]
