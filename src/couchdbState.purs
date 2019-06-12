module Perspectives.CouchdbState where

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Reader (ReaderT, lift, runReaderT)
import Data.Maybe (Maybe)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put, read, take, tryRead, new)

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
type UserInfo =
  { userName :: String
  , couchdbPassword :: String
  , couchdbBaseURL :: String
  }

-----------------------------------------------------------
-- COUCHDBSTATE
-----------------------------------------------------------
type CouchdbState f =
  { userInfo :: UserInfo
  , couchdbSessionStarted :: Boolean
  , sessionCookie :: AVar String
  | f
  }

type MonadCouchdb f = ReaderT (AVar (CouchdbState f)) Aff

-- | Run an action in MonadCouchdb, given a username and password.
runMonadCouchdb :: forall a. String -> String -> MonadCouchdb () a
  -> Aff a
runMonadCouchdb userName password mp = do
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  (rf :: AVar (CouchdbState ())) <- new $
    { userInfo:
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/" }
    , couchdbSessionStarted: false
    , sessionCookie: av
  }
  runReaderT mp rf

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall f. MonadCouchdb f Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: forall f. Boolean -> MonadCouchdb f Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: forall f. MonadCouchdb f (AVar String)
sessionCookie = gets _.sessionCookie

takeSessionCookieValue :: forall f. MonadCouchdb f String
takeSessionCookieValue = gets _.sessionCookie >>= lift <<< take

readSessionCookieValue :: forall f. MonadCouchdb f String
readSessionCookieValue = gets _.sessionCookie >>= lift <<< read

tryReadSessionCookieValue :: forall f. MonadCouchdb f (Maybe String)
tryReadSessionCookieValue = gets _.sessionCookie >>= lift <<< tryRead

setSessionCookie :: forall f. String -> MonadCouchdb f Unit
setSessionCookie c = sessionCookie >>= (lift <<< put c)
