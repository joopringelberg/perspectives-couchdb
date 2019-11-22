module Perspectives.Couchdb.Databases where

import Affjax (printResponseFormatError)
import Affjax (request, get, Request) as AJ
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError, catchJust)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, fromArray, fromObject, fromString)
import Data.Array (cons, find)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (fromFoldable, insert)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix, stripSuffix, toLower)
import Data.Tuple (Tuple(..))
import Effect.Aff (message)
import Effect.Aff.AVar (status, isEmpty, read)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error, error)
import Foreign (MultipleErrors)
import Foreign.Generic (decodeJSON)
import Foreign.Object (fromFoldable) as StrMap
import Perspectives.Couchdb (CouchdbStatusCodes, DBS, DatabaseName, DeleteCouchdbDocument, Password, User, escapeCouchdbDocumentName, onAccepted, onAccepted', onCorrectCallAndResponse)
import Perspectives.CouchdbState (MonadCouchdb, sessionCookie, setSessionCookie, takeSessionCookieValue)
import Perspectives.User (getCouchdbBaseURL, getUser, getCouchdbPassword)
import Prelude (Unit, bind, const, pure, show, unit, void, ($), (*>), (/=), (<$>), (<<<), (<>), (==), (>>=))

type ID = String

-----------------------------------------------------------
-- QUALIFYREQUEST
-----------------------------------------------------------
-- | Does not modify the request when:
-- |  * the sessionCookie AVar is empty;
-- |  * the value of the sessionCookie AVar is "Browser" (we don't do Authentication Cookies on the browser, as it
-- |    handles them itself.)
-- | Otherwise, adds a Cookie header containing the cached cookie. This is a synchronous function.
qualifyRequest :: forall f a. AJ.Request a -> MonadCouchdb f (AJ.Request a)
qualifyRequest req@{headers} = do
  -- cookie <- tryReadSessionCookieValue
  cookie <- pure Nothing
  case cookie of
    (Just x) | x /= "Browser" -> pure $ req {headers = cons (RequestHeader "Cookie" x) headers}
    otherwise -> pure req

defaultPerspectRequest :: forall f. MonadCouchdb f (AJ.Request String)
defaultPerspectRequest = qualifyRequest
  { method: Left GET
  , url: "http://localhost:5984/"
  , headers: []
  , content: Nothing
  -- TODO. Zonder de credentials weer mee te sturen, ben je niet geauthenticeerd.
  , username: Nothing
  , password: Nothing
  , withCredentials: true
  , responseFormat: ResponseFormat.string
}

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------
authenticate :: forall f. MonadCouchdb f Unit
authenticate = do
  b <- sessionCookie >>= lift <<< status >>= pure <<< isEmpty
  if b
    -- An authentication request is under way. Just wait till the AVar contains a value.
    then (sessionCookie >>= (void <<< lift <<< read))
    -- New authentication is necessary.
    else requestAuthentication

-- | To be called when the cookie is no longer valid.
requestAuthentication :: forall f. MonadCouchdb f Unit
requestAuthentication = do
  _ <- takeSessionCookieValue
  usr <- getUser
  pwd <- getCouchdbPassword
  requestAuthentication' usr pwd

-- | To be called if there is no cookie at all.
requestAuthentication' :: forall f. User -> Password -> MonadCouchdb f Unit
requestAuthentication' usr pwd = do
  base <- getCouchdbBaseURL
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left POST, url = (base <> "_session"), content = Just $ RequestBody.json (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))}
  -- (res :: AJ.AffjaxResponse PostCouchdb_session) <- lift $ AJ.post
    -- (base <> "_session")
    -- (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))
  case res.status of
    (StatusCode 200) -> saveCookie res.headers
    (StatusCode 203) -> saveCookie res.headers
    otherwise -> throwError $ error "Failure in authenticate. Unauthorized. Username or password wasn’t recognized"
  where
  -- In the browser, the cookie header is hidden from our code: the browser handles it by itself.
  saveCookie :: Array ResponseHeader -> MonadCouchdb f Unit
  saveCookie headers = case find (\rh -> toLower (name rh) == "set-cookie") headers of
    Nothing -> do
      setSessionCookie "Browser"
    (Just h) -> do
      -- NOTE. The Node implementation of Affjax depends on https://www.npmjs.com/package/xhr2. However, this emulation does not implement cookie authentication. Hence, we cannot use Perspectives from the command line.
      setSessionCookie $ value h

ensureAuthentication :: forall f a. MonadCouchdb f a -> MonadCouchdb f a
ensureAuthentication a = do
  b <- sessionCookie >>= lift <<< status >>= pure <<< isEmpty
  if b
    then (authenticate *> a) -- If empty, run authenticate and then run a.
    else (catchJust predicate a (const (authenticate *> a))) -- Otherwise, try a. When we then happen to be unauthenticated (cookie expired), run authenticate, then run a.
  where
    predicate :: Error -> Maybe Unit
    predicate e = if message e == "UNAUTHORIZED" then Just unit else Nothing

-- | A logout is purely client side, as Couchdb keeps no session state.
-- | (see: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie)
logout :: forall f. MonadCouchdb f Unit
logout = void takeSessionCookieValue

-----------------------------------------------------------
-- CREATE, DELETE, DATABASE
-----------------------------------------------------------
databaseStatusCodes :: CouchdbStatusCodes
databaseStatusCodes = fromFoldable
  [ Tuple 400 "Bad AJ.Request. Invalid database name."
  , Tuple 401 "Unauthorized. CouchDB Server Administrator privileges required."]

createDatabase :: forall f. DatabaseName -> MonadCouchdb f Unit
createDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> dbname)}
  -- (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit
  where
    createStatusCodes = insert 412 "Precondition failed. Database already exists."
      databaseStatusCodes

deleteDatabase :: forall f. DatabaseName -> MonadCouchdb f Unit
deleteDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left DELETE, url = (base <> dbname)}
  -- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' deleteStatusCodes res.status [200] "deleteDatabase" $ pure unit
  where
    deleteStatusCodes = insert 412 "Precondition failed. Database does not exist."
      databaseStatusCodes

-----------------------------------------------------------
-- CREATE, DELETE, USER
-----------------------------------------------------------
-- | Create a non-admin user.
createUser :: forall f. User -> Password -> Array Role -> MonadCouchdb f Unit
createUser user password roles = ensureAuthentication do
  base <- getCouchdbBaseURL
  rq <- defaultPerspectRequest
  content <- pure (fromObject (StrMap.fromFoldable
    [ Tuple "name" (fromString user)
    , Tuple "password" (fromString password)
    , Tuple "roles" (fromArray (fromString <$> roles))
    , Tuple "type" (fromString "user")]))
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_users/org.couchdb.user:" <> user), content = Just $ RequestBody.json content}
  liftAff $ onAccepted res.status [200, 201] "createUser" $ pure unit

type Role = String

-----------------------------------------------------------
-- SET SECURITY DOCUMENT
-----------------------------------------------------------
-- | Set the security document in the database.
setSecurityDocument :: forall f. DatabaseName -> Json -> MonadCouchdb f Unit
setSecurityDocument db doc = ensureAuthentication do
  base <- getCouchdbBaseURL
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> db <> "/_security"), content = Just $ RequestBody.json doc}
  liftAff $ onAccepted res.status [200, 201, 202] "setSecurityDocument" $ pure unit

-----------------------------------------------------------
-- SET DESIGN DOCUMENT
-----------------------------------------------------------
-- | Set the design document in the database.
setDesignDocument :: forall f. DatabaseName -> DocumentName -> Json -> MonadCouchdb f Unit
setDesignDocument db docname doc = ensureAuthentication do
  base <- getCouchdbBaseURL
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> db <> "/_design/" <> docname), content = Just $ RequestBody.json doc}
  liftAff $ onAccepted res.status [200, 201, 202] "setDesignDocument" $ pure unit

type DocumentName = String
-----------------------------------------------------------
-- ALLDBS
-----------------------------------------------------------
allDbs :: forall f. MonadCouchdb f (Array String)
allDbs = do
  base <- getCouchdbBaseURL
  res <- lift $ AJ.get ResponseFormat.string (base <> "_all_dbs")
  -- pure $ unwrap res.response
  case res.body of
    (Left r) -> throwError $ error ("allDbs: error in call: " <> printResponseFormatError r)
    (Right s) -> do
      (r :: Either MultipleErrors DBS) <- pure $ runExcept (decodeJSON s)
      case r of
        (Left e) -> throwError $ error ("allDbs: error in decoding result: " <> show e)
        (Right dbs) -> pure $ unwrap dbs

-----------------------------------------------------------
-- DOCUMENT EXISTS
-----------------------------------------------------------
documentExists :: forall f. ID -> MonadCouchdb f Boolean
documentExists url = do
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left HEAD, url = url}
  case res.status of
    StatusCode 200 -> pure true
    StatusCode 304 -> pure true
    otherwise -> pure false

-----------------------------------------------------------
-- DOCUMENT VERSION
-----------------------------------------------------------
retrieveDocumentVersion :: forall f. ID -> MonadCouchdb f String
retrieveDocumentVersion url = do
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left HEAD, url = url}
  -- _ <- log $ show res.headers
  -- _ <- log $ show res.status
  vs <- version res.headers
  liftAff $ onAccepted res.status [200, 304] "retrieveDocumentVersion" (pure vs)

  where
    version :: Array ResponseHeader -> MonadCouchdb f String
    version headers =  case find (\rh -> toLower (name rh) == "etag") headers of
      Nothing -> throwError $ error ("retrieveDocumentVersion: couchdb returns no ETag header holding a document version number for " <> url)
      (Just h) -> (pure $ value h) >>= removeDoubleQuotes

    removeDoubleQuotes :: String -> MonadCouchdb f String
    removeDoubleQuotes s = do
      ms1 <- pure $ stripPrefix (Pattern "\"") s
      case ms1 of
        Nothing -> throwError $ error ("retrieveDocumentVersion: couchdb returns ETag value that is not a valid JSON string for " <> url)
        (Just s1) -> do
          ms2 <- pure $ stripSuffix (Pattern "\"") s1
          case ms2 of
            Nothing -> throwError $ error ("retrieveDocumentVersion: couchdb returns ETag value that is not a double quoted string for " <> url)
            (Just s2) -> pure s2

-----------------------------------------------------------
-- ADD ATTACHMENT
-----------------------------------------------------------
addAttachment :: forall f. ID -> String -> String -> MediaType -> MonadCouchdb f DeleteCouchdbDocument
addAttachment docPath attachmentName attachment mimetype = do
  base <- getCouchdbBaseURL
  (rq@({headers}) :: (AJ.Request String)) <- defaultPerspectRequest
  rev <- retrieveDocumentVersion  (base <> docPath)
  res <- liftAff $ AJ.request $ rq
    { method = Left PUT
    , url = base <> docPath <> "/" <> escapeCouchdbDocumentName attachmentName <> "?rev=" <> rev
    , headers = cons (ContentType mimetype) headers
    , content = Just (RequestBody.string attachment)
    }
  onAccepted res.status [200, 201, 202] "addAttachment"
      (onCorrectCallAndResponse "addAttachment" res.body \(a :: DeleteCouchdbDocument)-> pure unit)
  -- For uploading attachments, the same structure is returned as for deleting a document.
