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

module Perspectives.Couchdb.ChangesFeed where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce', Emitter, emit, close)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)
import Foreign (Foreign, MultipleErrors)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.CouchdbState (MonadCouchdb)

-----------------------------------------------------------
-- TWO CONVENIENCE TYPES
-----------------------------------------------------------
-- Two useful types to make the type of x more compact.
-- emit :: forall m a r. Emitter m a r -> a -> m Unit
type EmitFunction m a r = Emitter m a r -> a -> m Unit

-- close :: forall m a r. Emitter m a r -> r -> m Unit
type CloseFunction m a r = Emitter m a r -> r -> m Unit

-----------------------------------------------------------
-- EVENTSOURCE
-----------------------------------------------------------
-- | Represents the type of instances of the (foreign) Javascript EventSource object.
-- | Use an EventSource to create a Producer of changes to a Couchdb database.
-- | Apply the function `closeEventSource` to finish the listener on the Javascript side.
foreign import data EventSource :: Type

foreign import createEventSourceImpl :: EffectFn2 FeedUrl QueryParams EventSource

-- | From a url (string, not terminated on a slash) that identifies a Couchdb database,
-- | create an EventSource that listens to changes in the database.
createEventSource :: FeedUrl -> QueryParams -> Effect EventSource
createEventSource feedUrl queryParams = runEffectFn2 createEventSourceImpl feedUrl queryParams

-----------------------------------------------------------
-- CLOSEEVENTSOURCE
-----------------------------------------------------------

foreign import closeEventSourceImpl :: EffectFn1 EventSource Unit

-- | Terminate the Couchdb change event stream.
closeEventSource :: EventSource -> Effect Unit
closeEventSource es = runEffectFn1 closeEventSourceImpl es

-----------------------------------------------------------
-- CHANGEPRODUCER
-----------------------------------------------------------
foreign import createChangeEmitterImpl :: forall f docType. EffectFn4
  EventSource
  (EmitFunction (MonadCouchdb f) (CouchdbChange docType) String)
  (CloseFunction (MonadCouchdb f) (CouchdbChange docType) String)
  (Emitter Effect (CouchdbChange docType) String)
  Unit

-- | Takes an EventSource and produces a function that takes an Emitter.
-- | Apply `produce` or `produce'` to it to create a Producer of CouchdbChange.
createChangeEmitter :: forall docType. EventSource -> (Emitter Effect (CouchdbChange docType) String) -> Effect Unit
createChangeEmitter eventSource = runEffectFn4 createChangeEmitterImpl
  eventSource
  emit
  -- \emitter e -> emit emitter (decodeCouchdbChange' e)
  close

-- | A Producer of `CouchdbChange`s. Each change to the database is emitted by the Producer.
changeProducer :: forall f docType. EventSource -> Producer (CouchdbChange docType) (MonadCouchdb f) String
changeProducer eventSource = produce' (createChangeEmitter eventSource)

-----------------------------------------------------------
-- COUCHDBCHANGE
-----------------------------------------------------------
-- TODO. Vervang dit door de Perspectives representatie van een verandering:
    -- {
    --   "seq": "22-g1AAAACTeJzLYWBgYMpgTmEQTM4vTc5ISXLIyU9OzMnILy7JAUklMiTV____PyuDOZEhFyjAnmaSaGFklJjCwFmal5KalpmXmoJHex4LkGRoAFL_oaaIgU0xN01JMzaxxKYvCwDEni2D",
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
newtype CouchdbChange docType = CouchdbChange
  { id :: String
  , changes :: Array ChangeRevision
  , deleted :: Boolean
  , doc :: docType
}

derive instance genericCouchdbChange :: Generic (CouchdbChange docType) _

derive instance newTypeCouchdbChange :: Newtype (CouchdbChange docType) _

showCouchdbChange :: forall docType. Show docType => CouchdbChange docType -> String
showCouchdbChange (CouchdbChange{id, deleted}) = "CouchdbChange: " <> id <> " (deleted=" <> show deleted <> ")"

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

type DecodedCouchdbChange docType = Either MultipleErrors (CouchdbChange docType)

decodeCouchdbChange' :: forall docType. Decode docType => Foreign -> DecodedCouchdbChange docType
decodeCouchdbChange' f = runExcept (decode f)

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
