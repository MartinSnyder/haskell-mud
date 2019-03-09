module World ( World
             , MessageDestination(..)
             , CommandArguments(..)
             , CommandEntry(..)
             , buildWorld
             , addPlayerIfAbsent
             , lookRoom -- remove
             , showInventory -- remove
             , doCommand
             , updateRoom
             , updateMob
             , updateWorld
             , sendMessageTo
             , extractOutput
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

data CommandArguments = CommandArguments { actor :: Mob
                                         , room :: Room
                                         , target1 :: Target
                                        , target2 :: Target
                                         , xtra :: String
                                         }

data CommandEntry = CommandEntry { name :: String
                                 , target1Spec :: Maybe (FindIn, [FindType])
                                 , target2Spec :: Maybe (FindIn, [FindType])
                                 , execute :: CommandArguments -> World -> Either String World
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
            broadcastText ((GameObj.sDesc mob) ++ " has entered the game") nextWorld'

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

updateWorld :: World -> [(World -> Either String World)] -> Either String World
updateWorld world ops =
    foldl (\ acc op -> acc >>= op) (Right world) ops

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
        (execute command) (CommandArguments actor room target1 target2 xtra) world

internalLookRoom :: UserId -> Room -> [Mob] -> World -> Either String World
internalLookRoom userId room mobs world =
    let
        elements = [ foldl (\acc s -> acc ++ " " ++ s) "Exits:" (fmap LinkDef.name $ RoomDef.links (Room.def room))
                   , foldl (\acc s -> acc ++ " " ++ s) "Occupants:" (fmap GameObj.sDesc mobs)
                   , foldl (\acc s -> acc ++ " " ++ s) "Items:" (fmap (GameObj.sDesc) (Room.items room))
                   ]
        text = foldl (\acc el -> acc ++ "\n" ++ el) (GameObj.lDesc room) elements
    in
        sendTextUser userId text world

lookRoom :: UserId -> World -> Either String World
lookRoom userId world =
    do
        mob <- getPlayer userId world
        room <- getRoom (locationId mob) world
        mobs <- mobIdsToMobs (mobIds room) world
        internalLookRoom userId room mobs world

showInventory :: UserId -> World -> Either String World
showInventory userId world =
    do
        connection <- getConnection userId world
        mob <- getPlayer userId world
        text <- return $ foldl (\acc s -> acc ++ " " ++ s) "Inventory:" (fmap (GameObj.sDesc) (Mob.items mob))
        sendTextUser userId text world

--------------------------
-- Routing text to players
--------------------------

getMobUserId :: MobId -> World -> Maybe UserId
getMobUserId mobId world =
    do
        mob <- Map.lookup mobId (mobs world)
        connectionId mob

sendTextUser :: UserId -> String -> World -> Either String World
sendTextUser userId text world =
    do
        connection <- getConnection userId world
        return world { connections = Map.insert userId (sendText text connection) $ World.connections world }

sendTextMobId :: MobId -> String -> World -> Either String World
sendTextMobId mobId text world =
    case getMobUserId mobId world of
        Just userId ->
            sendTextUser userId text world
        Nothing ->
            Right world

broadcastText :: String -> World -> Either String World
broadcastText message world =
    return world { connections = Map.map (sendText message) $ World.connections world }
    
-----------------------------
-- Formatted messaging system
-----------------------------

sendMessageMobId :: Mob -> Target -> Target -> String -> Message -> MobId -> World -> Either String World
sendMessageMobId actor target1 target2 xtra message mobId world =
    case getMobUserId mobId world of
        Just userId -> do
            connection <- getConnection userId world
            sendTextUser userId (resolveMessage actor target1 target2 xtra message connection) world
        Nothing ->
            Right world

sendMessageRoom :: Mob -> Target -> Target -> String -> Message -> Room -> World -> Either String World
sendMessageRoom actor target1 target2 xtra message room world =
    foldl (\ acc mob -> acc >>= sendMessageMobId actor target1 target2 xtra message mob) (Right world) $ Room.mobIds room

sendMessageTo :: CommandArguments -> MessageDestination -> Message -> World -> Either String World
sendMessageTo (CommandArguments actor room target1 target2 xtra) msgDest message world =
    case msgDest of
        MsgGlobal ->
            Right $ world { connections = Map.map (\ p -> sendText (resolveMessage actor target1 target2 xtra message p) p) $ World.connections world }
        MsgRoom ->
            sendMessageRoom actor target1 target2 xtra message room world
        MsgSpecificRoom targetRoomId ->
            do
                targetRoom <- getRoom targetRoomId world
                sendMessageRoom actor target1 target2 xtra message targetRoom world

-----------------------------
-- Retrieving output for players
-----------------------------

extractOutput :: UserId -> World -> (World, [String])
extractOutput userId world =
    case Map.lookup userId (World.connections world) of
        Just connection ->
            let
                userOutput = reverse . output $ connection
                nextConnection = connection { output = [] }
                nextWorld = world { connections = Map.insert userId nextConnection (World.connections world)}
            in (nextWorld, userOutput)
        Nothing ->
            (world, [])
