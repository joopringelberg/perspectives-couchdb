module Perspectives.CouchdbState where

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (new)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
type UserInfo =
  { userName :: UserName
  , couchdbPassword :: String
  , couchdbBaseURL :: String
  , userIdentifier :: String
  , _rev :: Maybe String
  }

newtype CouchdbUser = CouchdbUser UserInfo

derive instance genericRepCouchdbUser :: Generic CouchdbUser _

instance encodeCouchdbUser :: Encode CouchdbUser where
  encode = genericEncode defaultOptions

instance decodeCouchdbUser :: Decode CouchdbUser where
  decode = genericDecode defaultOptions

derive instance newtypeCouchdbUser :: Newtype CouchdbUser _

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
runMonadCouchdb userName password userId mp = do
  (rf :: AVar (CouchdbState ())) <- new $
    { userInfo: CouchdbUser
      { userName: UserName userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"
      , userIdentifier: userId
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
