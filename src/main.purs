module MainCouchdb (addAttachment, addAttachmentInDatabases) where

import Control.Promise (Promise, fromAff)
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Perspectives.Couchdb (DeleteCouchdbDocument)
import Perspectives.Couchdb.Databases (addAttachment, addAttachmentInDatabases) as PCD
import Perspectives.CouchdbState (runMonadCouchdb)
import Prelude (Unit, ($))

addAttachment_ :: String -> String -> String -> String -> Effect (Promise DeleteCouchdbDocument)
addAttachment_ docPath attachmentName attachment mimetype = fromAff $
  runMonadCouchdb "admin" "admin" "admin"
    (PCD.addAttachment docPath attachmentName attachment (MediaType mimetype))

addAttachment :: EffectFn4 String String String String (Promise DeleteCouchdbDocument)
addAttachment = mkEffectFn4 addAttachment_

addAttachmentInDatabases_ :: Array String -> String -> String -> String -> Effect (Promise Unit)
addAttachmentInDatabases_ docPaths attachmentName attachment mimetype = fromAff $
  runMonadCouchdb "admin" "admin" "admin"
    (PCD.addAttachmentInDatabases docPaths attachmentName attachment (MediaType mimetype))

addAttachmentInDatabases :: EffectFn4 (Array String) String String String (Promise Unit)
addAttachmentInDatabases = mkEffectFn4 addAttachmentInDatabases_
