module World ( World
             , MessageDestination(..)
             , CommandArguments(..)
             , CommandEntry(..)
             , RoomProcedure
             , buildWorld
             , addPlayerIfAbsent
             , getRoom
             , getPassage
             , getRoomMobs
             , updateRoom
             , updatePassage
             , updateMob
             , updateWorld
             , doCommand
             , sendTextMob
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
import PassageDef
import WorldDef
import PlayerData
import GameObj
import Room
import Mob
import Passage
import Connection
import Message
import Target

data MessageDestination = MsgGlobal | MsgActorRoom | MsgRoom DefId

data CommandArguments = CommandArguments { actor :: Mob
                                         , target1 :: Target
                                         , target2 :: Target
                                         , xtra :: String
                                         }

type CommandExecutor = CommandArguments -> World -> Either String World

data CommandEntry = CommandEntry { name :: String
                                 , target1Spec :: [FindType]
                                 , target2Spec :: [FindType]
                                 , execute :: CommandExecutor
                                 }

type RoomProcedure = CommandEntry -> CommandArguments -> World -> Either String World

data World = World { nextMobId :: MobId
                   , entryRoomId :: DefId
                   , rooms :: Map.Map DefId Room
                   , passages :: Map.Map DefId Passage
                   , mobs :: Map.Map MobId Mob
                   , roomProcs :: Map.Map String RoomProcedure
                   , connections :: Map.Map UserId Connection
                   }

roomListToMap :: [RoomDef] -> Map.Map DefId PassageDef -> Map.Map DefId Room
roomListToMap roomDefs passageDefs =
    foldl (\ acc roomDef -> Map.insert (GameDef.defId roomDef) (buildRoom roomDef passageDefs) acc) Map.empty roomDefs

passageListToMap :: [PassageDef] -> Map.Map DefId Passage
passageListToMap passageDefs =
    foldl (\ acc passageDef -> Map.insert (GameDef.defId passageDef) (buildPassage passageDef) acc) Map.empty passageDefs

buildWorld :: WorldDef -> Map.Map String RoomProcedure -> World
buildWorld worldDef roomProcs =
    let
        entryId = entry worldDef
        passages = passageListToMap $ passageDefs worldDef
        passageDefMap = foldl (\ acc passageDef -> Map.insert (GameDef.defId passageDef) passageDef acc) Map.empty $ passageDefs worldDef
        rooms = roomListToMap (roomDefs worldDef) passageDefMap
    in  World 0 entryId rooms passages Map.empty roomProcs Map.empty

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

getPassage :: DefId -> World -> Either String Passage
getPassage passageId world =
    case Map.lookup passageId $ passages world of
        Just passage -> Right passage
        Nothing -> Left $ "Cannot find passage " ++ show passageId

getRoomMobs :: Room -> World -> Either String [Mob]
getRoomMobs room world =
    mobIdsToMobs (mobIds room) world

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

updatePassage :: (Passage -> Either String Passage) -> DefId -> World -> Either String World
updatePassage f passageId world = do
    passage <- getPassage passageId world
    nextPassage <- f passage
    return world { passages = Map.insert (Passage.passageId nextPassage) nextPassage (passages world) }

updateMob :: (Mob -> Either String Mob) -> MobId -> World -> Either String World
updateMob f mobId world = do
    mob <- getMob mobId world
    nextMob <- f mob
    Right world { mobs = Map.insert (Mob.id nextMob) nextMob (mobs world) }

updateWorld :: World -> [(World -> Either String World)] -> Either String World
updateWorld world ops =
    foldl (\ acc op -> acc >>= op) (Right world) ops

resolveTarget :: [FindType] -> String -> Mob -> Room -> [Mob] -> Either String Target
resolveTarget [] _ _ _ _ = Right TargetNone
resolveTarget _ "" _ _ _ = Right TargetNone
resolveTarget findTypes keyword actor room roomMobs =
    case findTarget findTypes keyword actor room roomMobs of
        TargetNone -> Left $ "Could not find anything matching '" ++ keyword ++ ".'"
        target     -> Right target

-- Execute procedure tied to the current room
defaultRoomProc :: RoomProcedure
defaultRoomProc _ _ world = Right world

doRoomProc :: Room -> CommandEntry -> CommandArguments -> World -> Either String World
doRoomProc room command args world =
    let
        maybeProc = do
            name <- procName $ Room.def room
            Map.lookup name $ roomProcs world
        proc = fromMaybe defaultRoomProc maybeProc
    in  proc command args world

doCommand :: UserId -> CommandEntry -> String -> String -> String -> World -> Either String World
doCommand userId command keyword1 keyword2 xtra world =
    do
        actor <- getPlayer userId world
        room <- getRoom (locationId actor) world
        mobs <- mobIdsToMobs (mobIds room) world
        target1 <- resolveTarget (target1Spec command) keyword1 actor room mobs
        target2 <- resolveTarget (target2Spec command) keyword2 actor room mobs
        args <- Right $ CommandArguments actor target1 target2 xtra
        world' <- (execute command) args world
        doRoomProc room command args world'

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

sendTextMob :: Mob -> String -> World -> Either String World
sendTextMob mob text world =
    case getMobUserId (Mob.id mob) world of
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
sendMessageTo (CommandArguments actor target1 target2 xtra) msgDest message world =
    case msgDest of
        MsgGlobal ->
            Right $ world { connections = Map.map (\ p -> sendText (resolveMessage actor target1 target2 xtra message p) p) $ World.connections world }
        MsgActorRoom -> do
            targetRoom <- getRoom (locationId actor) world
            sendMessageRoom actor target1 target2 xtra message targetRoom world
        MsgRoom targetRoomId -> do
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
