module Item where

import GameDef
import GameObj
import ItemDef

data Item = Item { base :: ItemDef
                 } deriving (Show, Eq)

instance GameObj Item where
    sDesc item = GameDef.sDesc $ Item.base item
    lDesc item = GameDef.lDesc $ Item.base item
    matches item keyword = GameDef.matches (Item.base item) keyword

itemId :: Item -> DefId
itemId item = GameDef.defId (Item.base item)
    