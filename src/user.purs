module Perspectives.User where

import Control.Monad.AvarMonadAsk (gets, modify)
import Data.Newtype (over, unwrap)
import Perspectives.CouchdbState (CouchdbUser(..), MonadCouchdb, UserName(..))
import Prelude (Unit, bind, pure, show, ($), (<>), (>>>))

getUser :: forall f. MonadCouchdb f String
getUser = gets $ _.userInfo >>> unwrap >>> _.userName >>> unwrap

-- TODO. Verplaats naar de core.
-- | Returns a guid"
getSystemIdentifier :: forall f. MonadCouchdb f String
getSystemIdentifier = gets $ _.userInfo >>> unwrap >>> _.systemIdentifier

setUser :: forall f. String -> MonadCouchdb f Unit
setUser n = modify \ps@{userInfo: x@(CouchdbUser cur)} -> ps {userInfo = CouchdbUser cur {userName = UserName n}}

getCouchdbPassword :: forall f. MonadCouchdb f String
getCouchdbPassword = gets $ _.userInfo >>> unwrap >>> _.couchdbPassword

setCouchdbPassword :: forall f. String -> MonadCouchdb f Unit
setCouchdbPassword pw = modify \ps@{userInfo: x@(CouchdbUser cur)} -> ps {userInfo = CouchdbUser cur {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
-- TODO: construeer uit Host en Port.
getCouchdbBaseURL :: forall f. MonadCouchdb f String
getCouchdbBaseURL = do
  CouchdbUser{couchdbHost, couchdbPort} <- gets $ _.userInfo
  pure (couchdbHost <> ":" <> show couchdbPort <> "/")

-- Wordt nog niet gebruikt.
setCouchdbBaseURL :: forall f. String -> Int -> MonadCouchdb f Unit
setCouchdbBaseURL host port = modify \ps@{userInfo} -> ps {userInfo = over CouchdbUser (\cur -> cur {couchdbHost = host, couchdbPort = port}) userInfo}

getHost :: forall f. MonadCouchdb f String
getHost = gets $ _.userInfo >>> unwrap >>> _.couchdbHost

getPort :: forall f. MonadCouchdb f Int
getPort = gets $ _.userInfo >>> unwrap >>> _.couchdbPort
