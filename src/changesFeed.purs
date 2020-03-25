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

-- | This module defines a Producer (defined in Control.Coroutine) of documents that have changed in a
-- | Couchdb database. Deleted documents do not occur in the stream produced.
-- | The changes feed produced by Couchdb is the source of changes.
-- | The individual results are created by decoding the foreign results and can be
-- | either a list of multiple errors, or a document (the type of which needs to be an instance of Decode).
-- | Construct a Producer with an EventSource object created with `createEventSource`.

-- | In a future development, we'll capture the change itself instead of just the underlying document.

module Perspectives.Couchdb.ChangesFeed

  ( createEventSource
  , closeEventSource
  , includeDocs
  , changeProducer
  , EventSource
  , CouchdbChange(..)
  , ChangeRevision
  , ChangeProducer
  , DecodedCouchdbChange
  , decodeCouchdbChange')

  where

import Prelude

import Control.Coroutine (Producer, transform, ($~))
import Control.Coroutine.Aff (Emitter, Step(..), produce')
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)
import Foreign (Foreign, MultipleErrors)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.CouchdbState (MonadCouchdb)

-----------------------------------------------------------
-- FEEDURL, QUERYPARAMS
-----------------------------------------------------------
-- The database we want the changes feed from, not terminated in a slash.
type FeedUrl = String

-- | A string that will be appended to the url that is formed from the FeedUrl and the
-- | string "/_changes?feed=eventsource". Should be well formed, e.g. "&myparam=1"
-- |  See http://127.0.0.1:5984/_utils/docs/api/database/changes.html for the
-- | possible query arguments.
type QueryParams = String

-- | Include the associated document with each result. If there are conflicts,
-- | only the winning revision is returned
-- | (taken from: http://127.0.0.1:5984/_utils/docs/api/database/changes.html).
includeDocs :: QueryParams
includeDocs = "&include_docs=true"

-----------------------------------------------------------
-- EVENTSOURCE
-----------------------------------------------------------
-- | Represents the type of instances of the (foreign) Javascript EventSource object.
-- | Use an EventSource to create a Producer of changes to a Couchdb database.
-- | Apply the function `closeEventSource` to finish the listener on the Javascript side.
foreign import data EventSource :: Type

foreign import createEventSourceImpl :: EffectFn2 FeedUrl QueryParams EventSource

-- | From a URL (not a database name!) (string, not terminated on a slash) that identifies a Couchdb database,
-- | and a string with extra query parameters, create an EventSource that listens to changes in the database.
-- | Documents are included by default.
createEventSource :: FeedUrl -> QueryParams -> Effect EventSource
createEventSource feedUrl queryParams = runEffectFn2 createEventSourceImpl feedUrl (includeDocs <> queryParams)

-----------------------------------------------------------
-- CLOSEEVENTSOURCE
-----------------------------------------------------------
foreign import closeEventSourceImpl :: EffectFn1 EventSource Unit

-- | Terminate the Couchdb change event stream. Also terminates the Producer created with `changeProducer`.
closeEventSource :: EventSource -> Effect Unit
closeEventSource es = runEffectFn1 closeEventSourceImpl es

-----------------------------------------------------------
-- CHANGEPRODUCER
-----------------------------------------------------------
foreign import createChangeEmitterImpl :: EffectFn4
  EventSource
  (Foreign -> Step Foreign Unit)
  (Unit -> Step Foreign Unit)
  (Emitter Effect Foreign Unit)
  Unit

-- | Takes an EventSource and produces a function that takes an Emitter.
-- | Apply `produce` or `produce'` to it to create a Producer of Foreign.
createChangeEmitter ::
  EventSource ->
  (Emitter Effect Foreign Unit) ->
  Effect Unit
createChangeEmitter eventSource = runEffectFn4 createChangeEmitterImpl
  eventSource
  Emit
  Finish

type ChangeProducer f docType = Producer (Either MultipleErrors docType) (MonadCouchdb f) Unit

-- | A Producer of Foreign values. Each change to the database is emitted by the Producer.
changeProducer :: forall f docType. Decode docType => EventSource -> ChangeProducer f docType
changeProducer eventSource = (changeProducer' eventSource) $~ (forever (transform decodeDoc))

changeProducer' :: forall f. EventSource -> Producer Foreign (MonadCouchdb f) Unit
changeProducer' eventSource = produce' (createChangeEmitter eventSource)

decodeDoc :: forall docType. Decode docType => Foreign -> Either MultipleErrors docType
decodeDoc = runExcept <<< decode

-----------------------------------------------------------
-- DECODEDCOUCHDBCHANGE
-----------------------------------------------------------
-- | Is a CouchdbChange if it could be decoded correctly, a list of errors otherwise.
type DecodedCouchdbChange docType = Either MultipleErrors (CouchdbChange docType)

-- | The Foreign value is the encoded value of a CouchdbChange instance.
decodeCouchdbChange' :: forall docType. Decode docType => Foreign -> DecodedCouchdbChange docType
decodeCouchdbChange' f = runExcept (decode f)

-----------------------------------------------------------
-- COUCHDBCHANGE
-- This type will be used in a future release to capture changes rather than documents.
-- This depends on decoding the CouchdbChange object from the raw JSON form produced by Couchdb.
-- We cannot use the decode instance, because it was not encoded by its Encode instance.
-- Simple Json would be a good tool, were it not for the doc member. We encode all documents in Perspectives
-- using the generic encoding.
-----------------------------------------------------------
    -- {
    --   "seq": "22...",
    --   "id": "emptyTransaction",
    --   "changes": [
    --     {
    --       "rev": "2-f4769a3879540448d335a41418bd919d"
    --     }
    --   ],
    --   "deleted": true,
    --   "doc": {
    --     "_id": "emptyTransaction",
    --     "_rev": "2-f4769a3879540448d335a41418bd919d",
    --     "_deleted": true
    --   }
    -- }
-- { "seq": "7-"
-- ,"id": "test1"
-- ,"changes": [{"rev":"7-dcb195a59c23f1df8c30774dfa2cc910"}]
-- ,"doc":
--   { "_id": "test1"
--   ,"_rev": "7-dcb195a59c23f1df8c30774dfa2cc910"
--   ,"contents": {"test":"Hello world!"}
--   ,"tag": "TestDoc"}}
newtype CouchdbChange docType = CouchdbChange
  { id :: String
  , seq :: String
  , changes :: Array ChangeRevision
  , deleted :: Maybe Boolean
  , doc :: docType
  }

derive instance genericCouchdbChange :: Generic (CouchdbChange docType) _

derive instance newTypeCouchdbChange :: Newtype (CouchdbChange docType) _

instance showCouchdbChange :: Show docType => Show (CouchdbChange docType) where
  show (CouchdbChange{id}) = "CouchdbChange: " <> id
  -- show (CouchdbChange{id, deleted}) = "CouchdbChange: " <> id <> " (deleted=" <> show deleted <> ")"

instance decodeCouchdbChange :: Decode docType => Decode (CouchdbChange docType) where
  decode = genericDecode defaultOptions

instance encodeCouchdbChange :: Encode docType => Encode (CouchdbChange docType) where
  encode = genericEncode defaultOptions

newtype ChangeRevision = ChangeRevision { rev :: String}

derive instance genericChangeRevision :: Generic ChangeRevision _

instance decodeChangeRevision :: Decode ChangeRevision where
  decode = genericDecode defaultOptions

instance encodeChangeRevision :: Encode ChangeRevision where
  encode = genericEncode defaultOptions
