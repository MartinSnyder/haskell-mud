module RoomDef where

import Data.Map

import GameDef
import ItemDef
import LinkDef

data RoomDef = RoomDef { defId :: DefId
                       , name :: String
                       , desc :: String
                       , initialItems :: [ItemDef]
                       , links :: [LinkDef]
                       , procName :: Maybe String
                       } deriving (Show)

instance GameDef RoomDef where
    defId def = RoomDef.defId def
    sDesc def = RoomDef.name def
    lDesc def = (RoomDef.name def) ++ "\n" ++ (RoomDef.desc def)
    matches def keyword = False