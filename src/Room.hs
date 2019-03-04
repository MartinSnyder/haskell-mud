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
                 } deriving (Show)

roomId :: Room -> DefId
roomId room = GameDef.defId (def room)

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

removeItems :: [Item] -> Room -> Either String Room
removeItems items room =
    case List.partition (\el -> elem el items) (Room.items room) of
        (match, notMatch) -> if (length match == length items) then
            Right $ room { Room.items = notMatch }
        else
            Left $ "Not all specified items were found in the room " ++ show (roomId room)

findLink :: String -> Room -> Either String Link
findLink linkId room =
    case Map.lookup linkId $ links $ def room of
        Just link -> Right link
        Nothing -> Left $  "Link " ++ linkId ++ " not found in room " ++ show (roomId room)