module Perspectives.User where

import Control.Monad.AvarMonadAsk (gets, modify)
import Perspectives.CouchdbState (MonadCouchdb)
import Prelude (Unit, bind, ($), pure, (<>), (>>>))

getUser :: forall f. MonadCouchdb f String
getUser = gets $ _.userInfo >>> _.userName

setUser :: forall f. String -> MonadCouchdb f Unit
setUser n = modify \ps@{userInfo} -> ps {userInfo = userInfo {userName = n}}

getCouchdbPassword :: forall f. MonadCouchdb f String
getCouchdbPassword = gets $ _.userInfo >>> _.couchdbPassword

setCouchdbPassword :: forall f. String -> MonadCouchdb f Unit
setCouchdbPassword pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall f. MonadCouchdb f String
getCouchdbBaseURL = gets $ _.userInfo >>> _.couchdbBaseURL

setCouchdbBaseURL :: forall f. String -> MonadCouchdb f Unit
setCouchdbBaseURL pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbBaseURL = pw}}

-- | Url terminated with a forward slash.
entitiesDatabase :: forall f. MonadCouchdb f String
entitiesDatabase = do
  usr <- getUser
  cdbUrl <- getCouchdbBaseURL
  pure $ cdbUrl <> "user_" <> usr <> "_entities/"
