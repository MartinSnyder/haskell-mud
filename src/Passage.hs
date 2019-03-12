module Passage where

import GameDef
import GameObj
import PassageDef

data Passage = Passage { def :: PassageDef
                       , isClosed :: Bool
                       , isLocked :: Bool
                       }

instance GameObj Passage where
    sDesc passage = GameDef.sDesc $ Passage.def passage
    lDesc passage = GameDef.lDesc $ Passage.def passage
    matches passage keyword = GameDef.matches (Passage.def passage) keyword

buildPassage :: PassageDef -> Passage
buildPassage def =
    Passage def (PassageDef.isClosed $ passageType def) (PassageDef.isLocked $ passageType def)

passageId :: Passage -> DefId
passageId passage = GameDef.defId (def passage)
    