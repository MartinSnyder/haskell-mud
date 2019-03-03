module ItemDef where

import GameDef

data ItemDef = ItemDef { defId :: DefId
                       , sDesc :: String
                       , lDesc :: String
                       } deriving (Show, Eq)

instance GameDef ItemDef where
    defId def = ItemDef.defId def
    sDesc def = ItemDef.sDesc def
    lDesc def = ItemDef.lDesc def