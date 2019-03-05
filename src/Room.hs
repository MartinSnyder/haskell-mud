module Room where

import Data.List as List
import Data.Map as Map

import GameDef
import GameObj
import RoomDef
import Link
import Mob
import Item

data Room = Room { def :: RoomDef
                 , mobIds :: [MobId]
                 , items :: [Item]
                 , links :: Map String Link
                 } deriving (Show)

roomId :: Room -> DefId
roomId room = GameDef.defId (Room.def room)

instance GameObj Room where
    sDesc room = GameDef.sDesc $ Room.def room
    lDesc room = GameDef.lDesc $ Room.def room
    matches room keyword = GameDef.matches (Room.def room) keyword

addMobId :: MobId -> Room -> Either String Room
addMobId mobId room =
    Right $ room { mobIds = mobId : (mobIds room) }

removeMobId :: MobId -> Room -> Either String Room
removeMobId mobId room =
    case List.partition (== mobId) (mobIds room) of
        ([], _) -> Left $ "Mob " ++ show mobId ++ " not found in room " ++ show (roomId room)
        ([ foundMobId ], remainingMobIds) -> Right $ room { mobIds = remainingMobIds }

addItem :: Item -> Room -> Either String Room
addItem item room =
    Right $ room { Room.items = item : (Room.items room) }

removeItem :: Item -> Room -> Either String Room
removeItem item room =
    case List.partition (== item) (Room.items room) of
        ([], _) -> Left $ "Item " ++ (GameObj.sDesc item) ++ " not found in room " ++ show (roomId room)
        ([ foundItem ], remainingItems) -> Right $ room { Room.items = remainingItems }

findLink :: String -> Room -> Either String Link
findLink linkId room =
    case Map.lookup linkId $ Room.links room of
        Just link -> Right link
        Nothing -> Left $  "Link " ++ linkId ++ " not found in room " ++ show (roomId room)