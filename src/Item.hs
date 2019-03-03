module Item where

import GameDef
import GameObj
import ItemDef

data Item = Item { base :: ItemDef
                 } deriving (Show, Eq)

instance GameObj Item where
    sDesc mob = ItemDef.sDesc $ Item.base mob
    lDesc mob = ItemDef.lDesc $ Item.base mob