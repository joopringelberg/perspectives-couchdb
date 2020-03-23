module Perspectives.CouchdbState where

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (new)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision, changeRevision, getRev)

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
type UserInfo =
  { userName :: UserName
  , couchdbPassword :: String
  , couchdbHost :: String
  , couchdbPort :: Int
  , systemIdentifier :: String
  , _rev :: Maybe String
  }

newtype CouchdbUser = CouchdbUser UserInfo

derive instance genericRepCouchdbUser :: Generic CouchdbUser _

instance showCouchdbUser :: Show CouchdbUser where
  show = genericShow

instance eqCouchdbUser :: Eq CouchdbUser where
  eq = genericEq

instance encodeCouchdbUser :: Encode CouchdbUser where
  encode = genericEncode defaultOptions

instance decodeCouchdbUser :: Decode CouchdbUser where
  -- decode = genericDecode defaultOptions
  decode (json :: Foreign) = do
    rev <- getRev json
    a <- genericDecode defaultOptions json
    pure (changeRevision rev a)

derive instance newtypeCouchdbUser :: Newtype CouchdbUser _

instance revisionCouchdbUser :: Revision CouchdbUser where
  rev = _._rev <<< unwrap
  changeRevision s = over CouchdbUser (\vr -> vr {_rev = s})

newtype UserName = UserName String
derive instance newtypeUserName :: Newtype UserName _
derive instance genericRepUserName :: Generic UserName _
derive newtype instance encodeUserName :: Encode UserName
derive newtype instance decodeUserName :: Decode UserName
instance showUserName :: Show UserName where
  show = show <<< unwrap
instance eqUserName :: Eq UserName where
  eq (UserName id1) (UserName id2) = id1 == id2
instance ordUserName :: Ord UserName where
  compare (UserName a) (UserName b) = compare a b

-----------------------------------------------------------
-- COUCHDBSTATE
-----------------------------------------------------------
type CouchdbState f =
  { userInfo :: CouchdbUser
  , couchdbSessionStarted :: Boolean
  | f
  }

type MonadCouchdb f = ReaderT (AVar (CouchdbState f)) Aff

-- | Run an action in MonadCouchdb, given a username and password.
-- | Its primary use is in addAttachment_ (to add an attachment using the default "admin" account).
runMonadCouchdb :: forall a. String -> String -> String -> MonadCouchdb () a
  -> Aff a
runMonadCouchdb userName password systemId mp = do
  (rf :: AVar (CouchdbState ())) <- new $
    { userInfo: CouchdbUser
      { userName: UserName userName
      , couchdbPassword: password
      , couchdbHost: "http://127.0.0.1"
      , couchdbPort: 5984
      , systemIdentifier: systemId
      , _rev: Nothing}
    , couchdbSessionStarted: false
  }
  runReaderT mp rf

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall f. MonadCouchdb f Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: forall f. Boolean -> MonadCouchdb f Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}
