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

module MainCouchdb (addAttachment, addAttachmentInDatabases) where

import Control.Promise (Promise, fromAff)
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Perspectives.Couchdb (DeleteCouchdbDocument)
import Perspectives.Couchdb.Databases (addAttachment, addAttachmentInDatabases) as PCD
import Perspectives.CouchdbState (runMonadCouchdb)
import Prelude (Unit, ($))

addAttachment_ :: String -> String -> String -> String -> Effect (Promise DeleteCouchdbDocument)
addAttachment_ docPath attachmentName attachment mimetype = fromAff $
  runMonadCouchdb "admin" "admin" "admin" "http://127.0.0.1" 5984
    (PCD.addAttachment docPath attachmentName attachment (MediaType mimetype))

addAttachment :: EffectFn4 String String String String (Promise DeleteCouchdbDocument)
addAttachment = mkEffectFn4 addAttachment_

addAttachmentInDatabases_ :: Array String -> String -> String -> String -> Effect (Promise Unit)
addAttachmentInDatabases_ docPaths attachmentName attachment mimetype = fromAff $
  runMonadCouchdb "admin" "admin" "admin"  "http://127.0.0.1" 5984
    (PCD.addAttachmentInDatabases docPaths attachmentName attachment (MediaType mimetype))

addAttachmentInDatabases :: EffectFn4 (Array String) String String String (Promise Unit)
addAttachmentInDatabases = mkEffectFn4 addAttachmentInDatabases_
