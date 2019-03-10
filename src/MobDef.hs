module MobDef where

import GameDef
import Data.Set

data MobDef = MobDef { defId :: DefId
                     , sDesc :: String
                     , lDesc :: String
                     , keywords :: Set String
                     } deriving (Show, Read, Eq)

instance GameDef MobDef where
    defId def = MobDef.defId def
    sDesc def = MobDef.sDesc def
    lDesc def = MobDef.lDesc def
    matches def keyword = Data.Set.member keyword $ MobDef.keywords def