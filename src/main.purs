module Main (addAttachment) where

import Control.Promise (Promise, fromAff)
import Data.MediaType (MediaType)
import Effect (Effect)
import Perspectives.Couchdb (DeleteCouchdbDocument)
import Perspectives.Couchdb.Databases (addAttachment) as PCD
import Perspectives.CouchdbState (runMonadCouchdb)
import Prelude (($))

addAttachment :: String -> String -> String -> MediaType -> Effect (Promise DeleteCouchdbDocument)
addAttachment docPath attachmentName attachment mimetype = fromAff $
  runMonadCouchdb "admin" "admin"
    (PCD.addAttachment docPath attachmentName attachment mimetype)
