module Test.Utils where

import Prelude

import Effect.Aff (Aff)
import Perspectives.CouchdbState (MonadCouchdb, runMonadCouchdb)
import Test.Unit.Assert as Assert

runP :: forall a.
  MonadCouchdb () a ->
  Aff a
runP t = runMonadCouchdb "cor" "geheim" "cor" t

shouldEqual :: forall a. Eq a => a -> a -> Aff Boolean
shouldEqual a = \b -> pure (a == b)

type Message = String

assertEqual :: forall a. Eq a => Show a =>
  Message ->
  MonadCouchdb () a ->
  a ->
  Aff Unit
assertEqual message test result = do
  r <- runP test
  case result == r of
    true -> Assert.assert message true
    false -> Assert.assert (message <> "\nExpected: " <>
      show result <> "\nReceived: " <>
      show r)
      false
