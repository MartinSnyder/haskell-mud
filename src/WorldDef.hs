module WorldDef where

import GameDef
import RoomDef
import ItemDef
import LinkDef

data WorldDef = WorldDef { entry :: DefId
                         , roomDefs :: [RoomDef]
                         } deriving (Show, Read)