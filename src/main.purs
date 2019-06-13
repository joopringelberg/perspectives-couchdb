module Main (addAttachment) where

import Control.Promise (Promise, fromAff)
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Perspectives.Couchdb (DeleteCouchdbDocument)
import Perspectives.Couchdb.Databases (addAttachment) as PCD
import Perspectives.CouchdbState (runMonadCouchdb)
import Prelude (($))

addAttachment_ :: String -> String -> String -> String -> Effect (Promise DeleteCouchdbDocument)
addAttachment_ docPath attachmentName attachment mimetype = fromAff $
  runMonadCouchdb "admin" "admin"
    (PCD.addAttachment docPath attachmentName attachment (MediaType mimetype))

addAttachment :: EffectFn4 String String String String (Promise DeleteCouchdbDocument)
addAttachment = mkEffectFn4 addAttachment_
