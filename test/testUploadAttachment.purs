module Test.Couchdb.UploadAttachment (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect.Class.Console (logShow)
import Perspectives.Couchdb (DeleteCouchdbDocument(..), View(..), designDocumentViews)
import Perspectives.Couchdb.Databases (addAttachment, addView, addViewToDatabase, createDatabase, defaultDesignDocumentWithViewsSection, deleteDatabase, getDesignDocument, setDesignDocument)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Utils (assertEqual)

foreign import someview :: String

theSuite :: Free TestF Unit
theSuite = suite "addAttachment" do

  test "attach test.js to model:Perspectives" do
    assertEqual "We should be able to attach an attachment to model:Perspectives"
      ((addAttachment "perspect_models/model:Perspectives" "test.js" "function(){return 'hello world';}" (MediaType "text/ecmascript")) >>= \(DeleteCouchdbDocument {ok}) -> pure ok)
      (Just true)

  test "insert design document" do
    assertEqual "The retrieved document should equal the sent document"
      (do
        createDatabase "testdesigndocument"
        setDesignDocument "testdesigndocument" "defaultViews" (defaultDesignDocumentWithViewsSection "defaultViews")
        ddoc <- getDesignDocument "testdesigndocument" "defaultViews"
        logShow ddoc
        views <- pure $ designDocumentViews ddoc
        deleteDatabase "testdesigndocument"
        pure views)
      (designDocumentViews $ defaultDesignDocumentWithViewsSection "defaultViews")

  testOnly "insert view" do
    assertEqual "The retrieved document should equal the sent document"
      (do
        createDatabase "testdesigndocument"
        setDesignDocument "testdesigndocument" "defaultViews" (defaultDesignDocumentWithViewsSection "defaultViews")
        addViewToDatabase "testdesigndocument" "defaultViews" "someview" (View {map: someview, reduce: Nothing})
        ddoc <- getDesignDocument "testdesigndocument" "defaultViews"
        views <- pure $ designDocumentViews ddoc
        deleteDatabase "testdesigndocument"
        pure views)
      (designDocumentViews  (addView (defaultDesignDocumentWithViewsSection "defaultViews") "someview" (View {map: someview, reduce: Nothing})))
