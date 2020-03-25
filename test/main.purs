module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Couchdb.UploadAttachment (theSuite) as UA
import Test.Couchdb.ChangesFeed as CF

main :: Effect Unit
main = runTest do
  UA.theSuite
  CF.theSuite
