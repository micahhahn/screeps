module Screeps.Functions where

import Prelude

import Effect

import Screeps.Types
import Screeps.Internal.Foreign as F

constructionSiteRemove :: ConstructionSite -> Effect Unit
constructionSiteRemove (ConstructionSite c) = pure $ F.constructionSiteRemove c.handle

creepAttack :: Creep -> 

-- flagRemove :: Flag -> Effect ()
-- flagRemove = F.flagRemove 