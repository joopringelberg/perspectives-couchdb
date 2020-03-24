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
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn4, runEffectFn1, runEffectFn4)
import Perspectives.CouchdbState (MonadCouchdb)

-----------------------------------------------------------
-- CHANGEPRODUCER
-----------------------------------------------------------
-- Two useful types to make the type of x more compact.
-- emit :: forall m a r. Emitter m a r -> a -> m Unit
type EmitFunction m a r = Emitter m a r -> a -> m Unit

-- close :: forall m a r. Emitter m a r -> r -> m Unit
type CloseFunction m a r = Emitter m a r -> r -> m Unit

foreign import data EventSource :: Type

foreign import closeEventSourceImpl :: EffectFn1 EventSource Unit

-- | Terminate the Couchdb change event stream.
closeEventSource :: EventSource -> Effect Unit
closeEventSource es = runEffectFn1 closeEventSourceImpl es

-- Dit is de functie die we moeten importeren. We geven hem argumenten voor de eerste
-- drie parameters mee en dan hebben we een argument voor produce' zoals produceArgument.
-- Merk op dat het argument voor de parameter emitter door produce' wordt geleverd.
-- Dit is het type van Emitter:
-- newtype Emitter m a r = Emitter (Step a r -> m Unit)
-- Merk ook op dat we dit alleen maar doorgeven, nooit zelf uitpakken of iets anders
-- mee doen.

foreign import createChangeEmitterImpl :: forall f. EffectFn4
  FeedUrl
  (EmitFunction (MonadCouchdb f) CouchdbChange String)
  (CloseFunction (MonadCouchdb f) CouchdbChange String)
  (Emitter Effect CouchdbChange String)
  Unit

createChangeEmitter :: FeedUrl -> (Emitter Effect CouchdbChange String) -> Effect Unit
createChangeEmitter databaseUrl = runEffectFn4 createChangeEmitterImpl
  databaseUrl
  emit
  close

changeProducer :: forall f. FeedUrl -> Producer CouchdbChange (MonadCouchdb f) String
changeProducer feedUrl = produce' (createChangeEmitter feedUrl)

-- TODO. Vervang dit door de Perspectives representatie van een verandering:
-- {
--     "changes": [
--         {
--             "rev": "13-bcb9d6388b60fd1e960d9ec4e8e3f29e"
--         }
--     ],
--     "id": "SpaghettiWithMeatballs",
--     "seq":  "5-g1AAAAIReJyVkE0OgjAQRkcwUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloRid3MMkEUoJHbXbOxVy6arc_SxQWQzRVHCuYHaxSpuj1aqbj0t-3-AlSrZakn78oeSvjRSIkIhSNiCFHbsKN3c50b02mURvEB-yD296eNOzzoRMRLRZ98rkHS_veGcC_nR-fGe1gaCaxihhjOI2lX0BhniHaA"
-- }
type CouchdbChange = String

-- The database we want the changes feed from, not terminated in a slash.
type FeedUrl = String
