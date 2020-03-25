module Test.Couchdb.ChangesFeed (theSuite) where

import Prelude

import Control.Coroutine (Consumer, await, runProcess, ($$))
import Control.Monad.Except (lift)
import Control.Monad.Free (Free)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.ChangesFeed (ChangeProducer, changeProducer, closeEventSource, createEventSource)
import Perspectives.Couchdb.Databases (addDocument, createDatabase, deleteDatabase, deleteDocument)
import Perspectives.CouchdbState (MonadCouchdb)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Test.Utils (runP)

newtype TestDoc = TestDoc
  { test :: String }

derive instance genericTestDoc :: Generic TestDoc _

instance decodeTestDoc :: Decode TestDoc where
  decode = genericDecode defaultOptions

instance encodeTestDoc :: Encode TestDoc where
  encode = genericEncode defaultOptions

instance showTestDoc :: Show TestDoc where
  show = genericShow

theSuite :: Free TestF Unit
theSuite = suiteOnly "ChangesFeed" do

  test "Open and close an EventSource" (runP do
    -- Create a database
    -- createDatabase "testchangesfeed"
    -- Create an EventSource
    -- versie 1
    es <- liftEffect $ createEventSource "http://127.0.0.1:5984/testchangesfeed" ""
    -- Close the EventSource
    liftEffect $ closeEventSource es
    -- Delete the database
    deleteDatabase "testchangesfeed"
    liftAff $ assert "A changesfeed must be opened and closed." true
    )

  testOnly "Catch changes to a database" (runP do
    -- Create a database
    createDatabase "testchangesfeed"
    -- Create an EventSource
    es <- liftEffect $ createEventSource "http://127.0.0.1:5984/testchangesfeed" ""

    -- Create a producer.
    (myProducer :: ChangeProducer () TestDoc) <- pure $ changeProducer es

    -- We have to fork Aff, because the running process will block this thread.
    lift $ void $ forkAff (runP $ runProcess $ myProducer $$ consumeRequest)

    -- Add two documents
    addDocument "testchangesfeed" (TestDoc{test: "Hello world!"}) "test1"
    liftAff $ delay (Milliseconds 2000.0)
    addDocument "testchangesfeed" (TestDoc{test: "Hello world, again!"}) "test2"
    liftAff $ delay (Milliseconds 2000.0)

    -- Close the EventSource
    liftEffect $ closeEventSource es

    -- Delete the document
    -- void $ deleteDocument "http://127.0.0.1:5984/testchangesfeed/test1" Nothing

    -- Delete the database
    deleteDatabase "testchangesfeed"
    liftAff $ assert "A ChangeProducer must be created" true
    )

consumeRequest :: Consumer (Either MultipleErrors TestDoc) (MonadCouchdb ()) Unit
consumeRequest = forever do
  change <- await
  log "A change has occurred"
  logShow change
