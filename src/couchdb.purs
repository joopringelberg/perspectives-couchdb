-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Couchdb where

import Affjax (ResponseFormatError, printResponseFormatError)
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, fromString, jsonSingletonObject)
import Data.Array (elemIndex, find)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Foreign (F, Foreign, MultipleErrors, unsafeFromForeign)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Foreign.JSON (parseJSON)
import Foreign.Object (Object, fromFoldable, empty) as OBJ
import Perspectives.Couchdb.Revision (class Revision, changeRevision, getRev)
import Prelude (class Eq, class Show, Unit, bind, pure, show, ($), (*>), (<$>), (<<<), (<>), (==))
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- ALIASES
-----------------------------------------------------------
-- | URL terminated with a forward slash, e.g. http://www.perspectives.nl/.
type TerminatedURL = String
type User = String
type Password = String
type DatabaseName = String
type Key = String

-----------------------------------------------------------
-- PUTCOUCHDBDOCUMENT
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

instance revisionPutCouchdbDocument :: Revision PutCouchdbDocument where
  rev = _.rev <<< unwrap
  changeRevision s d = d

-----------------------------------------------------------
-- DELETECOUCHDBDOCUMENT
-----------------------------------------------------------
newtype DeleteCouchdbDocument = DeleteCouchdbDocument
  { ok :: Maybe Boolean
  , id :: Maybe String
  , rev :: Maybe String}

derive instance genericDeleteCouchdbDocument :: Generic DeleteCouchdbDocument _

derive instance newtypeDeleteCouchdbDocument :: Newtype DeleteCouchdbDocument _

instance decodeDeleteCouchdbDocument :: Decode DeleteCouchdbDocument where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance revisionDeleteCouchdbDocument :: Revision DeleteCouchdbDocument where
  rev = _.rev <<< unwrap
  changeRevision s d = d

-----------------------------------------------------------
-- REPLICATIONDOCUMENT
-----------------------------------------------------------
-- {
--     "_id": "my_rep",
--     "source": "http://myserver.com/foo",
--     "target":  "http://user:pass@localhost:5984/bar",
--     "create_target":  true,
--     "continuous": true
-- }
newtype ReplicationDocument = ReplicationDocument
  { _id :: String
  , source :: String
  , target :: String
  , create_target :: Boolean
  , continuous :: Boolean
  , selector :: Maybe SelectorObject
  }

derive instance genericReplicationDocument :: Generic ReplicationDocument _

derive instance newtypeReplicationDocument :: Newtype ReplicationDocument _

instance showReplicationDocument :: Show ReplicationDocument where
  show = genericShow

instance decodeReplicationDocument :: Decode ReplicationDocument where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeReplicationDocument :: Encode ReplicationDocument where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- We use the EncodeJson instance in setReplicationDocument.
instance encodeJsonReplicationDocument :: EncodeJson ReplicationDocument where
    encodeJson (ReplicationDocument ddr) = unsafeCoerce $ write ddr

instance revisionReplicationDocument :: Revision ReplicationDocument where
  rev v = Nothing
  changeRevision s d = d

-----------------------------------------------------------
-- ATTACHMENTS
-----------------------------------------------------------
  -- {
  --     "content_type": "text/plain",
  --     "digest": "md5-Ids41vtv725jyrN7iUvMcQ==",
  --     "length": 1872,
  --     "revpos": 4,
  --     "stub": true
  -- }
type AttachmentRecord =
  { content_type :: String
  , digest :: String
  , length :: Int
  , revpos :: Int
  , stub :: Boolean
  }

type AttachmentInfo = OBJ.Object AttachmentRecord

newtype DocWithAttachmentInfo = DocWithAttachmentInfo DocWithAttachmentInfoR

type DocWithAttachmentInfoR = {_attachments :: AttachmentInfo}

instance showDocWithAttachmentInfo :: Show DocWithAttachmentInfo where
  show (DocWithAttachmentInfo r) = show r

instance decodeDocWithAttachmentInfo :: Decode DocWithAttachmentInfo where
  -- decode f = readString f >>= map DocWithAttachmentInfo <<< readJSON'
  decode f = pure $ DocWithAttachmentInfo $ unsafeFromForeign f

instance revisionDocWithAttachmentInfo :: Revision DocWithAttachmentInfo where
  rev _ = Nothing
  changeRevision s d = d

-----------------------------------------------------------
-- SELECTOROBJECT
-----------------------------------------------------------
-- | We support simple SelectorObjects that specify one or more fields with a required value.
-- | Notice that we specify a subfield selector structure. This is because we anticipate
-- | the use of genericEncode to encode objects. This means that, instead of being a toplevel
-- | key, the key of interest is a key in the object with key "contents".
type SelectorObject = OBJ.Object (OBJ.Object String)

selectOnField :: String -> String -> SelectorObject
selectOnField key value = OBJ.fromFoldable [Tuple "contents" (OBJ.fromFoldable [Tuple key value])]

emptySelector :: SelectorObject
emptySelector = OBJ.fromFoldable [Tuple "contents" OBJ.empty]
-----------------------------------------------------------
-- DBS
-----------------------------------------------------------
newtype DBS = DBS (Array String)
derive instance genericDBS :: Generic DBS _
derive instance newtypeDBS :: Newtype DBS _
instance decodeDBS :: Decode DBS where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- GETCOUCHDBALLDOCS
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

instance revisionGetCouchdbAllDocs :: Revision GetCouchdbAllDocs where
  rev _ = Nothing
  changeRevision s d = d

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
-- DESIGN DOCUMENT
-----------------------------------------------------------
-- {
--     "_id": "_design/application",
--     "_rev": "1-C1687D17",
--     "views": {
--         "viewname": {
--             "map": "function(doc) { ... }",
--             "reduce": "function(keys, values) { ... }"
--         }
--     }
-- }
newtype DesignDocument = DesignDocument DesignDocumentRecord

type DesignDocumentRecord =
  { _id :: String
  , _rev :: Maybe String
  , views :: OBJ.Object View
}

derive instance genericDesignDocument :: Generic DesignDocument _
derive instance newtypeDesignDocument :: Newtype DesignDocument _
instance decodeDesignDocument :: Decode DesignDocument where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeDesignDocument :: Encode DesignDocument where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance showDesignDocument :: Show DesignDocument where
  show = genericShow
instance encodeJonDesignDocument :: EncodeJson DesignDocument where
  -- encodeJson (DesignDocument ddr) = encodeJson ddr
  encodeJson (DesignDocument{_id, _rev, views})= case _rev of
    Nothing -> fromObject $ OBJ.fromFoldable
      [ Tuple "_id" (fromString _id)
      , Tuple "views" (fromObject (encodeJson <$> views))
      ]
    Just r -> fromObject $ OBJ.fromFoldable
      [ Tuple "_id" (fromString _id)
      , Tuple "_rev" (fromString r)
      , Tuple "views" (fromObject (encodeJson <$> views))
      ]

instance revisionDesignDocument :: Revision DesignDocument where
  rev = _._rev <<< unwrap
  changeRevision s d = d

designDocumentViews :: DesignDocument -> OBJ.Object View
designDocumentViews = _.views <<< unwrap

-----------------------------------------------------------
-- DESIGN DOCUMENT
-----------------------------------------------------------
newtype View = View
  { map :: String
  , reduce :: Maybe String
}

derive instance genericView :: Generic View _
derive instance newtypeView :: Newtype View _
instance decodeView :: Decode View where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeView :: Encode View where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqView :: Eq View where
  eq = genericEq
instance showView :: Show View where
  show = genericShow
instance encodeJonView :: EncodeJson View where
  encodeJson (View{map,reduce}) = case reduce of
    Nothing -> jsonSingletonObject "map" (fromString map)
    Just r -> fromObject $ OBJ.fromFoldable
      [ Tuple "map" (fromString map)
      , Tuple "reduce" (fromString r)
      ]

-----------------------------------------------------------
-- VIEWRESULT
-----------------------------------------------------------
-- {
--     "offset": 0,
--     "rows": [
--         {
--             "id": "SpaghettiWithMeatballs",
--             "key": "meatballs",
--             "value": 1
--         },
--         {
--             "id": "SpaghettiWithMeatballs",
--             "key": "spaghetti",
--             "value": 1
--         },
--         {
--             "id": "SpaghettiWithMeatballs",
--             "key": "tomato sauce",
--             "value": 1
--         }
--     ],
--     "total_rows": 3
-- }

newtype ViewResult f = ViewResult
  { offset :: Int
  , rows :: Array (ViewResultRow f)
  , total_rows :: Int
  }

instance revisionViewResult :: Revision (ViewResult f) where
  rev _ = Nothing
  changeRevision s d = d

newtype ViewResultRow f = ViewResultRow { id :: String, key :: String, value :: f }

derive instance genericViewResult :: Generic (ViewResult f) _
derive instance newtypeViewResult :: Newtype (ViewResult f) _
instance decodeViewResult :: Decode f => Decode (ViewResult f) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

derive instance genericViewResultRow :: Generic (ViewResultRow f) _
derive instance newtypeViewResultRow :: Newtype (ViewResultRow f) _
instance decodeViewResultRow :: Decode f => Decode (ViewResultRow f) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
  -- Decodeer het eerste niveau. Pas decode to op f: dan wordt de revisie gezet.
  -- decode = readString >=> parseJSON >=> decode

-----------------------------------------------------------
-- SECURITY DOCUMENT
-----------------------------------------------------------
-- {
--     "admins": {
--         "names": [
--             "Bob"
--         ],
--         "roles": []
--     },
--     "members": {
--         "names": [
--             "Mike",
--             "Alice"
--         ],
--         "roles": []
--     }
-- }
newtype SecurityDocument = SecurityDocument
  { admins :: { names :: Array String, roles :: Array String}
  , members :: { names :: Array String, roles :: Array String}
}

derive instance genericSecurityDocument :: Generic SecurityDocument _
derive instance newtypeSecurityDocument :: Newtype SecurityDocument _
instance decodeSecurityDocument :: Decode SecurityDocument where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSecurityDocument :: Encode SecurityDocument where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance showSecurityDocument :: Show SecurityDocument where
  show = genericShow

instance encodeJonSecurityDocument :: EncodeJson SecurityDocument where
  encodeJson (SecurityDocument r) = encodeJson r

instance decodeJonSecurityDocument :: DecodeJson SecurityDocument where
  decodeJson j = SecurityDocument <$> decodeJson j

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

onCorrectCallAndResponse :: forall a m. MonadError Error m => Decode a => Revision a => String -> Either ResponseFormatError String -> (a -> m Unit) -> m a
onCorrectCallAndResponse n (Left e) _ = throwError $ error (n <> ": error in call: " <> printResponseFormatError e)
onCorrectCallAndResponse n (Right r) f = do
  (x :: Either MultipleErrors a) <- pure $ runExcept (decodeResource r)
  case x of
    (Left e) -> do
      throwError $ error (n <> ": error in decoding result: " <> show e)
    (Right result) -> f result *> pure result
  where
    decodeResource :: String -> F a
    -- decodeResource = parseJSON >=> decode
    decodeResource s = do
      (json :: Foreign) <- parseJSON s
      rev <- getRev json
      a <- decode json
      pure $ (changeRevision rev) a

escapeCouchdbDocumentName :: String -> String
escapeCouchdbDocumentName s = replaceAll (Pattern ":") (Replacement "%3A") (replaceAll (Pattern "$") (Replacement "%24") s)

version :: forall m. MonadError Error m => Array ResponseHeader -> m (Maybe String)
version headers =  case find (\rh -> toLower (name rh) == "etag") headers of
  Nothing -> throwError $ error ("Perspectives.Instances.version: couchdb returns no ETag header holding a document version number")
  (Just h) -> case runExcept $ decodeJSON (value h) of
    Left e -> pure Nothing
    Right v -> pure $ Just v
