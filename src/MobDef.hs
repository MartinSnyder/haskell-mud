module MobDef where

import GameDef

data MobDef = MobDef { defId :: DefId
                     , sDesc :: String
                     , lDesc :: String
                     } deriving (Show, Eq)

instance GameDef MobDef where
    defId def = MobDef.defId def
    sDesc def = MobDef.sDesc def
    lDesc def = MobDef.lDesc def