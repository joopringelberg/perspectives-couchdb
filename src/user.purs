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

module Perspectives.User where

import Control.Monad.AvarMonadAsk (gets, modify)
import Data.Newtype (unwrap)
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
getCouchdbPassword = gets _.couchdbPassword

-- | Url terminated with a forward slash.
-- TODO: construeer uit Host en Port.
getCouchdbBaseURL :: forall f. MonadCouchdb f String
getCouchdbBaseURL = do
  couchdbHost <- gets $ _.couchdbHost
  couchdbPort <- gets $ _.couchdbPort
  pure (couchdbHost <> ":" <> show couchdbPort <> "/")

getHost :: forall f. MonadCouchdb f String
getHost = gets $ _.couchdbHost

getPort :: forall f. MonadCouchdb f Int
getPort = gets $ _.couchdbPort
