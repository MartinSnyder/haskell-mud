module World ( World
             , buildWorld
             , extractOutput
             , addPlayerIfAbsent
             , followLink
             , sendBroadcastMessage
             , sendGlobalMessage
             , sendLocalMessage
             , lookRoom
             ) where

import Data.List
import qualified Data.Map as Map

import GameDef
import MobDef
import ItemDef
import RoomDef
import PlayerData
import GameObj
import Room
import Link
import Mob
import Item
import Connection
import Message

data World = World { nextMobId :: MobId
                   , entryRoomId :: DefId
                   , rooms :: Map.Map DefId Room
                   , mobs :: Map.Map MobId Mob
                   , connections :: Map.Map UserId Connection
                   } deriving (Show)

buildRoom :: RoomDef -> Room
buildRoom roomDef =
    Room roomDef [] items
    where items = fmap (\itemDef -> Item itemDef) (RoomDef.initialItems roomDef)

roomListToMap :: [RoomDef] -> Map.Map DefId Room
roomListToMap roomDefs =
    foldl (\ acc roomDef -> Map.insert (GameDef.defId roomDef) (buildRoom roomDef) acc) Map.empty roomDefs

buildWorld :: DefId -> [RoomDef] -> World
buildWorld entry roomDefs =
    World 0 entry (roomListToMap roomDefs) Map.empty Map.empty

getEntry :: World -> Room
getEntry world =
    case Map.lookup (entryRoomId world) (rooms world) of
        Just room -> room
        Nothing -> error "Cannot enter a world with an invalid entry"

addMob :: Either MobDef PlayerData -> DefId -> Maybe UserId -> World -> Either String (Mob, World)
addMob mobBase roomId connectionId world =
    let
        mobId = nextMobId world
        newMob = Mob mobId roomId connectionId mobBase
        nextWorld = world { nextMobId = (nextMobId world) + 1
                          , mobs = Map.insert mobId newMob $ mobs world
                          }
    in
        do
            nextWorld' <- updateRoom (addMobId mobId) (entryRoomId nextWorld) nextWorld
            return (newMob, nextWorld')

addConnection :: UserId -> MobId -> World -> Either String (Connection, World)
addConnection userId mobId world =
    let
        newConnection = Connection userId mobId []
    in
        Right ( newConnection
              , world { connections = Map.insert userId newConnection $ World.connections world }
              )

addPlayerIfAbsent :: UserId -> World-> Either String World
addPlayerIfAbsent userId world =
    case getPlayer userId world of
        Right _ -> Right world
        Left _ -> do
            playerData <- Right $ PlayerData userId userId
            (mob, nextWorld) <- addMob (Right playerData) (entryRoomId world) (Just userId) world
            (connection, nextWorld') <- addConnection userId (Mob.id mob) nextWorld
            sendBroadcastMessage ((GameObj.sDesc mob) ++ " has entered the game") nextWorld'

getPlayer :: UserId -> World -> Either String Mob
getPlayer userId world = do
    connection <- getConnection userId world
    getMob (Connection.mobId connection) world

getConnection :: UserId -> World -> Either String Connection
getConnection userId world =
    case Map.lookup userId $ connections world of
        Just connection -> Right connection
        Nothing -> Left $ "Cannot find connection for user " ++ userId

getRoom :: DefId -> World -> Either String Room
getRoom roomId world =
    case Map.lookup roomId $ rooms world of
        Just room -> Right room
        Nothing -> Left $ "Cannot find room " ++ show roomId

getMob :: MobId -> World -> Either String Mob
getMob mobId world =
    case Map.lookup mobId $ mobs world of
        Just mob -> Right mob
        Nothing -> Left $ "Cannot find mob " ++ show mobId

mobIdsToMobs :: [MobId] -> World -> Either String [Mob]
mobIdsToMobs mobIds world =
    foldr (folder world) (Right []) mobIds
    where folder world id accEither = do
                                        acc <- accEither
                                        mob <- getMob id world
                                        return $ mob : acc
updateRoom :: (Room -> Either String Room) -> DefId -> World -> Either String World
updateRoom f roomId world = do
    room <- getRoom roomId world
    nextRoom <- f room
    return world { rooms = Map.insert (Room.roomId nextRoom) nextRoom (rooms world) }

updateMob :: (Mob -> Either String Mob) -> MobId -> World -> Either String World
updateMob f mobId world = do
    mob <- getMob mobId world
    nextMob <- f mob
    Right world { mobs = Map.insert (Mob.id nextMob) nextMob (mobs world) }

routeMessage :: UserId -> String -> World -> Either String World
routeMessage userId message world =
    do
        connection <- getConnection userId world
        return world { connections = Map.insert userId (sendMessage message connection) $ World.connections world }

sendMessageMobId :: Mob -> Maybe Mob -> String -> Message -> MobId -> World -> Either String World
sendMessageMobId actor possiblyTarget xtra message mobId world =
    let
        maybeUserId = do
            mob <- Map.lookup mobId (mobs world)
            connectionId mob
    in case maybeUserId of
        Just userId -> do
            connection <- getConnection userId world
            routeMessage userId (resolveMessage actor possiblyTarget xtra message connection) world
        Nothing ->
            Right world

sendMessageRoom :: Mob -> Maybe Mob -> String -> Message -> Room -> World -> Either String World
sendMessageRoom actor possiblyTarget xtra message room world =
    foldl (\ acc mob -> acc >>= sendMessageMobId actor possiblyTarget xtra message mob) (Right world) $ Room.mobIds room

sendMessageRoomId :: MobId -> Maybe String -> String -> Message -> DefId -> World -> Either String World
sendMessageRoomId actorId possiblyTargetId xtra message roomId world =
    do
        room <- getRoom roomId world
        actor <- getMob actorId world
        sendMessageRoom actor Nothing xtra message room world

internalLookRoom :: Room -> [Mob] -> Connection -> World -> Either String World
internalLookRoom room mobs connection world =
    let
        elements = [ foldl (\acc s -> acc ++ " " ++ s) "Exits:" (Map.keys $ links (def room))
                   , foldl (\acc s -> acc ++ " " ++ s) "Occupants:" (fmap (GameObj.sDesc) mobs)
                   , foldl (\acc s -> acc ++ " " ++ s) "Items:" (fmap (GameObj.sDesc) (Room.items room))
                   ]
        text = foldl (\acc el -> acc ++ "\n" ++ el) (GameObj.lDesc room) elements
        nextConnection = sendMessage text connection
    in
        Right world { connections = Map.insert (Connection.userId nextConnection) nextConnection $ World.connections world }

lookRoom :: UserId -> World -> Either String World
lookRoom userId world =
    do
        connection <- getConnection userId world
        mob <- getPlayer userId world
        room <- getRoom (locationId mob) world
        mobs <- mobIdsToMobs (mobIds room) world
        internalLookRoom room mobs connection world

updateWorld :: World -> [(World -> Either String World)] -> Either String World
updateWorld world ops =
    foldl (\ acc op -> acc >>= op) (Right world) ops

followLink :: UserId -> String -> World -> Either String World
followLink userId linkId world =
    do
        mob <- getPlayer userId world
        sourceRoom <- getRoom (locationId mob) world
        link <- findLink linkId sourceRoom
        targetRoom <- getRoom (targetRoomId link) world
        updateWorld world [ updateRoom (removeMobId $ Mob.id mob) $ roomId sourceRoom
                          , updateRoom (addMobId $ Mob.id mob) $ (roomId targetRoom)
                          , sendMessageRoomId (Mob.id mob) Nothing "" exitMessage (roomId sourceRoom)
                          , sendMessageRoomId (Mob.id mob) Nothing (GameObj.sDesc targetRoom) enterMessage (roomId targetRoom)
                          , updateMob (\mob -> Right $ mob { locationId = (roomId targetRoom)}) (Mob.id mob)
                          ]
    where
        exitMessage = [Actor, ActorVerb "leave" "left", Const "the room."]
        enterMessage = [Actor, ActorVerb "enter" "enters", Xtra False, Const "."]

sendBroadcastMessage :: String -> World -> Either String World
sendBroadcastMessage message world =
    return world { connections = Map.map (sendMessage message) $ World.connections world }

sendGlobalMessage :: UserId -> Maybe String -> String -> Message -> World -> Either String World
sendGlobalMessage userId possiblyTarget xtra message world = do
    actor <- getPlayer userId world
    return world { connections = Map.map (\ p -> sendMessage (resolveMessage actor Nothing xtra message p) p) $ World.connections world }

sendLocalMessage :: UserId -> Maybe String -> String -> Message -> World -> Either String World
sendLocalMessage userId possiblyTarget xtra message world =
    do
        actor <- getPlayer userId world
        sendMessageRoomId (Mob.id actor) possiblyTarget xtra message (locationId actor) world

extractOutput :: UserId -> World -> (World, [String])
extractOutput userId world =
    case Map.lookup userId (World.connections world) of
        Just connection ->
            let
                messages = reverse . Connection.messages $ connection
                nextConnection = connection { messages = [] }
                nextWorld = world { connections = Map.insert userId nextConnection (World.connections world)}
            in (nextWorld, messages)
        Nothing ->
            (world, [])