module Perspectives.Couchdb where

import Affjax (ResponseFormatError, printResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Array (elemIndex)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Prelude (Unit, show, ($), (<>), (==), bind, pure, (*>))

-----------------------------------------------------------
-- ALIASES
-----------------------------------------------------------
-- | URL terminated with a forward slash, e.g. http://www.perspectives.nl/.
type TerminatedURL = String
type User = String
type Password = String
type DatabaseName = String

-----------------------------------------------------------
-- DOCUMENT
-----------------------------------------------------------

newtype PutCouchdbDocument = PutCouchdbDocument
  { ok :: Maybe Boolean
  , id :: Maybe String
  , rev :: Maybe String
  , error :: Maybe String
  , reason :: Maybe String}

derive instance genericPutCouchdbDocument :: Generic PutCouchdbDocument _

derive instance newtypePutCouchdbDocument :: Newtype PutCouchdbDocument _

instance decodePutCouchdbDocument :: Decode PutCouchdbDocument where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype DeleteCouchdbDocument = DeleteCouchdbDocument
  { ok :: Maybe Boolean
  , id :: Maybe String
  , rev :: Maybe String}

derive instance genericDeleteCouchdbDocument :: Generic DeleteCouchdbDocument _

derive instance newtypeDeleteCouchdbDocument :: Newtype DeleteCouchdbDocument _

instance decodeDeleteCouchdbDocument :: Decode DeleteCouchdbDocument where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- DBS
-----------------------------------------------------------
newtype DBS = DBS (Array String)
derive instance genericDBS :: Generic DBS _
derive instance newtypeDBS :: Newtype DBS _
instance decodeDBS :: Decode DBS where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- DB
-----------------------------------------------------------
newtype GetCouchdbAllDocs = GetCouchdbAllDocs
  { offset :: Int
  , rows :: Array DocReference
  , total_rows :: Int
  , update_seq :: Maybe Int
  }

derive instance genericCouchdbAllDocs :: Generic GetCouchdbAllDocs _
derive instance newtypeCouchdbAllDocs :: Newtype GetCouchdbAllDocs _

instance decodeGetCouchdbAllDocs :: Decode GetCouchdbAllDocs where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype DocReference = DocReference { id :: String, value :: Rev}

derive instance genericDocReference :: Generic DocReference _

instance decodeDocReference :: Decode DocReference where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype Rev = Rev { rev :: String}

derive instance genericRef :: Generic Rev _

instance decodeRev :: Decode Rev where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- AUTHENTICATION
-----------------------------------------------------------
newtype PostCouchdb_session = PostCouchdb_session
  { ok :: Boolean
  , name :: Maybe User
  , roles :: Array String
  }

derive instance genericPostCouchdb_session :: Generic PostCouchdb_session _
derive instance newtypePostCouchdb_session :: Newtype PostCouchdb_session _
instance decodePostCouchdb_session :: Decode PostCouchdb_session where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- STATUS CODES
-----------------------------------------------------------

type CouchdbStatusCodes = Map Int String

couchdDBStatusCodes :: CouchdbStatusCodes
couchdDBStatusCodes = fromFoldable
  [ Tuple 200 "OK. Request completed successfully"
  , Tuple 201 "Created. Document created successfully."
  , Tuple 202 "Accepted. Request has been accepted, but the corresponding operation may not have completed. This is used for background operations, such as database compaction."
  , Tuple 304 "Not Modified. The additional content requested has not been modified. This is used with the ETag system to identify the version of information returned."
  , Tuple 400 "Bad Request. Bad request structure. The error can indicate an error with the request URL, path or headers. Differences in the supplied MD5 hash and content also trigger this error, as this may indicate message corruption."
  , Tuple 401 "Unauthorized. The item requested was not available using the supplied authorization, or authorization was not supplied."
  , Tuple 403 "Forbidden. The requested item or operation is forbidden."
  , Tuple 404 "Not found. The requested content could not be found. The content will include further information, as a JSON object, if available. The structure will contain two keys, error and reason."
  , Tuple 405 "Resource not allowed. A request was made using an invalid HTTP request type for the URL requested. For example, you have requested a PUT when a POST is required. Errors of this type can also triggered by invalid URL strings."
  , Tuple 406 "Not acceptable. The requested content type is not supported by the server."
  , Tuple 409 "Conflict. Request resulted in an update conflict."
  , Tuple 412 "Precondition failed. The request headers from the client and the capabilities of the server do not match."
  , Tuple 415 "Bad Content type. The content types supported, and the content type of the information being requested or submitted indicate that the content type is not supported."
  , Tuple 416 "Requested Range not satisfiable. The range specified in the request header cannot be satisfied by the server."
  , Tuple 417 "Expectation failed. When sending documents in bulk, the bulk load operation failed."
  , Tuple 500 "Internal server error. The request was invalid, either because the supplied JSON was invalid, or invalid information was supplied as part of the request."
  ]

-- | Throws an error pertinent to the statuscode if it is not one of the statusCodes that are acceptable.
-- | Otherwise evaluates f.
-- onAccepted :: forall m a. MonadError Error m => StatusCode -> Array Int -> String -> m a -> m a
onAccepted :: forall a m. MonadError Error m => StatusCode -> Array Int -> String -> m a -> m a
onAccepted (StatusCode n) statusCodes fname f = case elemIndex n statusCodes of
  Nothing -> handleError n mempty fname
  otherwise -> f

onAccepted' :: forall a m. MonadError Error m => CouchdbStatusCodes -> StatusCode -> Array Int -> String -> m a -> m a
onAccepted' specialCodes (StatusCode n) statusCodes fname f = case elemIndex n statusCodes of
  Nothing -> do
    handleError n specialCodes fname
  otherwise -> f

handleError :: forall a m. MonadError Error m => Int -> CouchdbStatusCodes -> String -> m a
handleError n statusCodes fname =
  if n == 401
    then throwError $ error "UNAUTHORIZED"
    else
      case lookup n statusCodes of
        (Just m) -> throwError $ error $  "Failure in " <> fname <> ". " <> m
        Nothing ->
          case lookup n couchdDBStatusCodes of
            (Just m) -> throwError $ error $  "Failure in " <> fname <> ". " <> m
            Nothing -> throwError $ error $ "Failure in " <> fname <> ". " <> "Unknown HTTP statuscode " <> show n

onCorrectCallAndResponse :: forall a m. MonadError Error m => Decode a => String -> Either ResponseFormatError String -> (a -> m Unit) -> m a
onCorrectCallAndResponse n (Left e) _ = throwError $ error (n <> ": error in call: " <> printResponseFormatError e)
onCorrectCallAndResponse n (Right r) f = do
  (x :: Either MultipleErrors a) <- pure $ runExcept (decodeJSON r)
  case x of
    (Left e) -> throwError $ error (n <> ": error in decoding result: " <> show e)
    (Right result) -> f result *> pure result

escapeCouchdbDocumentName :: String -> String
escapeCouchdbDocumentName s = replaceAll (Pattern ":") (Replacement "%3A") (replaceAll (Pattern "$") (Replacement "%24") s)
