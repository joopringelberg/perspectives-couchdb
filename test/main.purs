module Test.Main where

import Prelude

import Effect (Effect)
import Test.Couchdb.UploadAttachment (theSuite) as UA
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  UA.theSuite
