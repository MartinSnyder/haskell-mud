module World ( World
             , MessageDestination(..)
             , CommandPayload(..)
             , CommandEntry(..)
             , buildWorld
             , extractOutput
             , addPlayerIfAbsent
             , followLink
             , sendBroadcastMessage
             , sendGlobalMessage
             , sendLocalMessage
             , sendMessageTo
             , lookRoom
             , doCommand
             , showInventory
             , getItem
             , dropItem
             , updateRoom
             , updateMob
             , updateWorld
             ) where

import Data.List
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import GameDef
import MobDef
import ItemDef
import LinkDef
import RoomDef
import PlayerData
import GameObj
import Room
import Link
import Mob
import Item
import Connection
import Message
import Target

data MessageDestination = MsgGlobal | MsgRoom | MsgSpecificRoom DefId

data CommandPayload = CommandPayload { actor :: Mob
                                     , room :: Room
                                     , target1 :: Target
                                     , target2 :: Target
                                     , xtra :: String
                                     }

data CommandEntry = CommandEntry { name :: String
                                 , target1Spec :: Maybe (FindIn, [FindType])
                                 , target2Spec :: Maybe (FindIn, [FindType])
                                 , execute :: CommandPayload -> World -> Either String World
                                 }

data World = World { nextMobId :: MobId
                   , entryRoomId :: DefId
                   , rooms :: Map.Map DefId Room
                   , mobs :: Map.Map MobId Mob
                   , connections :: Map.Map UserId Connection
                   } deriving (Show)

buildRoom :: RoomDef -> Room
buildRoom roomDef =
    let
        links = Map.fromList $ fmap (\linkDef -> (map toLower $ LinkDef.name linkDef, Link linkDef)) (RoomDef.links roomDef)
        items = fmap (\itemDef -> Item itemDef) (RoomDef.initialItems roomDef)
    in
        Room roomDef [] items links

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
        newMob = Mob mobId roomId [] connectionId mobBase
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

sendMessageMobId :: Mob -> Target -> Target -> String -> Message -> MobId -> World -> Either String World
sendMessageMobId actor target1 target2 xtra message mobId world =
    let
        maybeUserId = do
            mob <- Map.lookup mobId (mobs world)
            connectionId mob
    in case maybeUserId of
        Just userId -> do
            connection <- getConnection userId world
            routeMessage userId (resolveMessage actor target1 target2 xtra message connection) world
        Nothing ->
            Right world

sendMessageRoom :: Mob -> Target -> Target -> String -> Message -> Room -> World -> Either String World
sendMessageRoom actor target1 target2 xtra message room world =
    foldl (\ acc mob -> acc >>= sendMessageMobId actor target1 target2 xtra message mob) (Right world) $ Room.mobIds room

sendMessageRoomId :: MobId -> Target -> Target -> String -> Message -> DefId -> World -> Either String World
sendMessageRoomId actorId target1 target2 xtra message roomId world =
    do
        room <- getRoom roomId world
        actor <- getMob actorId world
        sendMessageRoom actor target1 target2 xtra message room world

internalLookRoom :: Room -> [Mob] -> Connection -> World -> Either String World
internalLookRoom room mobs connection world =
    let
        elements = [ foldl (\acc s -> acc ++ " " ++ s) "Exits:" (fmap LinkDef.name $ RoomDef.links (Room.def room))
                   , foldl (\acc s -> acc ++ " " ++ s) "Occupants:" (fmap GameObj.sDesc mobs)
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

resolveTarget :: Maybe (FindIn, [FindType]) -> String -> Mob -> Room -> [Mob] -> Target
resolveTarget targetSpec keyword actor room roomMobs =
    fromMaybe TargetNone $ fmap (\(findIn, findTypes) -> findTarget findIn findTypes keyword actor room roomMobs) targetSpec

doCommand :: UserId -> CommandEntry -> String -> String -> String -> World -> Either String World
doCommand userId command keyword1 keyword2 xtra world =
    do
        actor <- getPlayer userId world
        room <- getRoom (locationId actor) world
        mobs <- mobIdsToMobs (mobIds room) world
        target1 <- return $ resolveTarget (target1Spec command) keyword1 actor room mobs
        target2 <- return $ resolveTarget (target2Spec command) keyword2 actor room mobs
        (execute command) (CommandPayload actor room target1 target2 xtra) world

showInventory :: UserId -> World -> Either String World
showInventory userId world =
    do
        connection <- getConnection userId world
        mob <- getPlayer userId world
        text <- return $ foldl (\acc s -> acc ++ " " ++ s) "Inventory:" (fmap (GameObj.sDesc) (Mob.items mob))
        nextConnection <- return $ sendMessage text connection
        Right world { connections = Map.insert (Connection.userId nextConnection) nextConnection $ World.connections world }

getItem :: UserId -> String -> World -> Either String World
getItem userId keyword world =
    do
        actor <- getPlayer userId world
        sourceRoom <- getRoom (locationId actor) world
        item <- case findTarget FindInRoom [FindItem] keyword actor sourceRoom [] of
            TargetItem item -> Right item
            _ -> Left $ "Could not find anything matching '" ++ keyword ++ ".'"
        updateWorld world [ updateRoom (Room.removeItem item) $ roomId sourceRoom
                          , updateMob (Mob.addItem item) (Mob.id actor)
                          , sendMessageRoomId (Mob.id actor) (TargetItem item) TargetNone "" message (roomId sourceRoom)
                          ]
    where
        message = [ActorDesc, ActorVerb "take" "takes", Target1Desc, Const "."]

dropItem :: UserId -> String -> World -> Either String World
dropItem userId keyword world =
    do
        actor <- getPlayer userId world
        sourceRoom <- getRoom (locationId actor) world
        item <- case findTarget FindInActor findAllTypes keyword actor sourceRoom [] of
            TargetItem item -> Right item
            _ -> Left $ "Could not find anything matching '" ++ keyword ++ ".'"
        updateWorld world [ updateRoom (Room.addItem item) $ roomId sourceRoom
                          , updateMob (Mob.removeItem item) (Mob.id actor)
                          , sendMessageRoomId (Mob.id actor) (TargetItem item) TargetNone "" message (roomId sourceRoom)
                          ]
    where
        message = [ActorDesc, ActorVerb "drop" "drops", Target1Desc, Const "."]

updateWorld :: World -> [(World -> Either String World)] -> Either String World
updateWorld world ops =
    foldl (\ acc op -> acc >>= op) (Right world) ops

followLink :: UserId -> String -> World -> Either String World
followLink userId keyword world =
    do
        actor <- getPlayer userId world
        sourceRoom <- getRoom (locationId actor) world
        link <- case findTarget FindInRoom [FindLink] keyword actor sourceRoom [] of
            TargetLink link -> Right link
            _ -> Left $ "Can't find an exit called '" ++ keyword ++ ".'"
        targetRoom <- getRoom (targetRoomId $ Link.def link) world
        updateWorld world [ updateRoom (removeMobId $ Mob.id actor) $ roomId sourceRoom
                          , updateRoom (addMobId $ Mob.id actor) $ (roomId targetRoom)
                          , sendMessageRoomId (Mob.id actor) (TargetLink link) TargetNone "" exitMessage (roomId sourceRoom)
                          , sendMessageRoomId (Mob.id actor) (TargetLink link) TargetNone (GameObj.sDesc targetRoom) enterMessage (roomId targetRoom)
                          , updateMob (\mob -> Right $ mob { locationId = (roomId targetRoom)}) (Mob.id actor)
                          ]
    where
        exitMessage = [ActorDesc, ActorVerb "leave" "left", Const "the room."]
        enterMessage = [ActorDesc, ActorVerb "enter" "enters", Xtra False, Const "."]

sendBroadcastMessage :: String -> World -> Either String World
sendBroadcastMessage message world =
    return world { connections = Map.map (sendMessage message) $ World.connections world }

sendGlobalMessage :: UserId -> Target -> Target -> String -> Message -> World -> Either String World
sendGlobalMessage userId target1 target2 xtra message world = do
    actor <- getPlayer userId world
    return world { connections = Map.map (\ p -> sendMessage (resolveMessage actor target1 target2 xtra message p) p) $ World.connections world }

sendLocalMessage :: UserId -> Target -> Target -> String -> Message -> World -> Either String World
sendLocalMessage userId target1 target2 xtra message world =
    do
        actor <- getPlayer userId world
        sendMessageRoomId (Mob.id actor) target1 target2 xtra message (locationId actor) world

sendMessageTo :: CommandPayload -> MessageDestination -> Message -> World -> Either String World
sendMessageTo (CommandPayload actor room target1 target2 xtra) msgDest message world =
    case msgDest of
        MsgGlobal ->
            Right $ world { connections = Map.map (\ p -> sendMessage (resolveMessage actor target1 target2 xtra message p) p) $ World.connections world }
        MsgRoom ->
            sendMessageRoomId (Mob.id actor) target1 target2 xtra message (locationId actor) world
        MsgSpecificRoom targetRoomId ->
            sendMessageRoomId (Mob.id actor) target1 target2 xtra message targetRoomId world

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