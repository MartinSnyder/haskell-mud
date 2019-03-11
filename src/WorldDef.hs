module WorldDef where

import GameDef
import RoomDef
import ItemDef
import LinkDef
import PassageDef

data WorldDef = WorldDef { entry :: DefId
                         , roomDefs :: [RoomDef]
                         , passageDefs :: [PassageDef]
                         } deriving (Show, Read)