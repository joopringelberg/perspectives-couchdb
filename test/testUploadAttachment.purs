module Test.Couchdb.UploadAttachment (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Perspectives.Couchdb (DeleteCouchdbDocument(..))
import Perspectives.Couchdb.Databases (addAttachment)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Utils (assertEqual)

theSuite :: Free TestF Unit
theSuite = suiteSkip "addAttachment" do
  test "attach test.js to model:Perspectives" do
    assertEqual "We should be able to attach an attachment to model:Perspectives"
      ((addAttachment "perspect_models/model:Perspectives" "test.js" "function(){return 'hello world';}" (MediaType "text/ecmascript")) >>= \(DeleteCouchdbDocument {ok}) -> pure ok)
      (Just true)
