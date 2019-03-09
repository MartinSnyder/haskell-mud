module CommandInterpreter (Command, parseCommand, applyCommand) where

import Data.List (elemIndex, take, drop, sort, find)
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.String.Utils (strip, startswith)

import World
import Mob
import Link
import LinkDef
import GameObj
import Room
import RoomDef
import Message
import PlayerData
import Target

data Command = Noop
             | Invalid String
             | ExecuteCommand CommandEntry String String String

commandList =   [ CommandEntry "help" Nothing Nothing (\ _ _ ->
                    let commands = sort $ map World.name commandList
                    in  Left $ foldl (\acc s -> acc ++ " " ++ s) "Available Commands:" commands
                )
                , CommandEntry "go" (Just (FindInRoom, [FindLink])) Nothing (\ args world -> do
                    link <- asLink $ target1 args
                    destinationRoomId <- return $ targetRoomId $ Link.def link
                    updateWorld world [ updateRoom (removeMobId $ Mob.id $ actor args) $ roomId $ room args
                                        , updateRoom (addMobId $ Mob.id $ actor args) $ destinationRoomId
                                        , sendMessageTo args MsgRoom [ActorDesc, ActorVerb "leave" "left", Const "the room."]
                                        , sendMessageTo args (MsgSpecificRoom destinationRoomId) [ActorDesc, ActorVerb "enter" "enters", Xtra False, Const "."]
                                        , updateMob (\mob -> Right $ mob { locationId = destinationRoomId }) $ Mob.id $ actor args
                                        ]
                )
                , CommandEntry "get" (Just (FindInRoom, [FindItem])) Nothing (\ args world -> do
                    item <- asItem $ target1 args
                    updateWorld world [ updateRoom (Room.removeItem item) $ roomId $ room args
                                        , updateMob (Mob.addItem item) $ Mob.id $ actor args
                                        , sendMessageTo args MsgRoom [ActorDesc, ActorVerb "take" "takes", Target1Desc, Const "."]
                                        ]
                )
                , CommandEntry "drop" (Just (FindInActor, [FindItem])) Nothing (\ args world -> do
                    item <- asItem $ target1 args
                    updateWorld world [ updateRoom (Room.addItem item) $ roomId $ room args
                                        , updateMob (Mob.removeItem item) $ Mob.id $ actor args
                                        , sendMessageTo args MsgRoom [ActorDesc, ActorVerb "drop" "drops", Target1Desc, Const "."]
                                        ]
                )
                , CommandEntry "say" Nothing Nothing (\ args world ->
                    if xtra args == "" then Left $ "What do you want to say?"
                    else                    sendMessageTo args MsgRoom [ActorDesc, ActorVerb "say" "says", Xtra True] world
                )
                , CommandEntry "inventory" Nothing Nothing (\ args world ->
                    let text = foldl (\acc s -> acc ++ " " ++ s) "Inventory:" (fmap (GameObj.sDesc) (Mob.items $ actor args))
                    in  sendTextMob (actor args) text world
                )
                , CommandEntry "look" Nothing Nothing (\ args world ->
                    do
                        room <- return $ World.room args
                        mobs <- getRoomMobs room world
                        elements <- return [ foldl (\acc s -> acc ++ " " ++ s) "Exits:" (fmap LinkDef.name $ RoomDef.links (Room.def room))
                                            , foldl (\acc s -> acc ++ " " ++ s) "Occupants:" (fmap GameObj.sDesc mobs)
                                            , foldl (\acc s -> acc ++ " " ++ s) "Items:" (fmap (GameObj.sDesc) (Room.items room))
                                            ]
                        text <- return $ foldl (\acc el -> acc ++ "\n" ++ el) (GameObj.lDesc room) elements
                        sendTextMob (actor args) text world
                )
                , CommandEntry "yell" Nothing Nothing (\ args world ->
                    if xtra args == "" then Left $ "What do you want to yell?"
                    else                    sendMessageTo args MsgGlobal [ActorDesc, ActorVerb "yell" "yells", Xtra True] world
                )
                , CommandEntry "bow" (Just (FindInRoom, findAllTypes)) Nothing (\ args world ->
                    case target1 args of
                        TargetNone -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "bow" "bows", Const "deeply."] world
                        _          -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "bow" "bows", Const "to ", Target1Desc, Const "."] world
                )
                , CommandEntry "clap" (Just (FindInRoom, findAllTypes)) Nothing (\ args world ->
                    case target1 args of
                        TargetNone -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "clap" "claps", Const "enthusiastically."] world
                        _          -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "clap" "claps", Const "at ", Target1Desc, Const "."] world
                )
                , CommandEntry "jump" (Just (FindInRoom, findAllTypes)) Nothing (\ args world ->
                    case target1 args of
                        TargetNone   -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "jump" "jumps", Const "as high as they can!"] world
                        TargetItem _ -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "jump" "jumps", Const "over ", Target1Desc, Const "."] world
                        TargetMob _  -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "get" "gets", Const "ready to jump over ", Target1Desc, Const " but thinks better of it."] world
                        TargetLink _ -> sendMessageTo args MsgRoom [ActorDesc, ActorVerb "jump" "jumps", Const "towards ", Target1Desc, Const " slowly."] world
                )
                ]

commandMap = Map.fromList $ map (\cmd -> (World.name cmd, cmd)) commandList

buildCommand :: CommandEntry -> String -> Command
buildCommand command rest =
    let
        (keyword1, rest') = case target1Spec command of
            Just _ -> getFirstWord rest
            Nothing -> ("", rest)

        (keyword2, rest'') = case target2Spec command of
            Just _ -> getFirstWord rest'
            Nothing -> ("", rest)
    in
        ExecuteCommand command keyword1 keyword2 rest''

parseCommand :: String -> Command
parseCommand "" = Noop
parseCommand line =
    let
        -- Important: This is the entry point for all input. Before even looking at it, we:
        -- 1. Convert to lowercase AND
        -- 2. Strip boundary whitespace
        (firstWord, rest) = getFirstWord . strip $ map toLower line
    in
        case Map.lookup firstWord commandMap of
            Just command -> buildCommand command rest
            Nothing -> case find (\cmd -> startswith firstWord $ World.name cmd) commandList of
                Just command -> buildCommand command rest
                Nothing -> Invalid line

getFirstWord :: String -> (String, String)
getFirstWord line = case firstSpace of
    Just index -> (take index line, strip $ drop (index + 1) line)
    Nothing -> (line, [])
    where firstSpace = elemIndex ' ' line

applyCommand :: UserId -> Command -> World -> Either String World
applyCommand userId command world =
    case command of
        Noop ->
            Right world
        Invalid text ->
            Left $ "Invalid input: " ++ text
        ExecuteCommand cmdEntry keyword1 keyword2 extra ->
            doCommand userId cmdEntry keyword1 keyword2 extra world