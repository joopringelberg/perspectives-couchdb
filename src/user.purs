module Perspectives.User where

import Control.Monad.AvarMonadAsk (gets, modify)
import Data.Newtype (over, unwrap)
import Perspectives.CouchdbState (CouchdbUser(..), MonadCouchdb, UserName(..))
import Prelude (Unit, ($), (>>>))

getUser :: forall f. MonadCouchdb f String
getUser = gets $ _.userInfo >>> unwrap >>> _.userName >>> unwrap

getUserIdentifier :: forall f. MonadCouchdb f String
getUserIdentifier = gets $ _.userInfo >>> unwrap >>> _.userIdentifier

setUser :: forall f. String -> MonadCouchdb f Unit
setUser n = modify \ps@{userInfo: x@(CouchdbUser cur)} -> ps {userInfo = CouchdbUser cur {userName = UserName n}}

getCouchdbPassword :: forall f. MonadCouchdb f String
getCouchdbPassword = gets $ _.userInfo >>> unwrap >>> _.couchdbPassword

setCouchdbPassword :: forall f. String -> MonadCouchdb f Unit
setCouchdbPassword pw = modify \ps@{userInfo: x@(CouchdbUser cur)} -> ps {userInfo = CouchdbUser cur {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall f. MonadCouchdb f String
getCouchdbBaseURL = gets $ _.userInfo >>> unwrap >>> _.couchdbBaseURL

setCouchdbBaseURL :: forall f. String -> MonadCouchdb f Unit
setCouchdbBaseURL pw = modify \ps@{userInfo} -> ps {userInfo = over CouchdbUser (\cur -> cur {couchdbBaseURL = pw}) userInfo}
