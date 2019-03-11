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
                    updateWorld world [ updateRoom (removeMobId $ Mob.id $ actor args) $ locationId $ actor args
                                      , updateRoom (addMobId $ Mob.id $ actor args) $ destinationRoomId
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "leave" "left", Const "the room."]
                                      , sendMessageTo args (MsgRoom destinationRoomId) [Desc Actor, Sur " " $ Verb Actor "enter" "enters", Xtra, Const "."]
                                      , updateMob (\mob -> Right $ mob { locationId = destinationRoomId }) $ Mob.id $ actor args
                                      ]
                )
                , CommandEntry "get" (Just (FindInRoom, [FindItem])) Nothing (\ args world -> do
                    item <- asItem $ target1 args
                    updateWorld world [ updateRoom (Room.removeItem item) $ locationId $ actor args
                                      , updateMob (Mob.addItem item) $ Mob.id $ actor args
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "take" "takes", Desc Target1, Const "."]
                                      ]
                )
                , CommandEntry "drop" (Just (FindInActor, [FindItem])) Nothing (\ args world -> do
                    item <- asItem $ target1 args
                    updateWorld world [ updateRoom (Room.addItem item) $ locationId $ actor args
                                      , updateMob (Mob.removeItem item) $ Mob.id $ actor args
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "drop" "drops", Desc Target1, Const "."]
                                      ]
                )
                , CommandEntry "give" (Just (FindInActor, [FindItem])) (Just (FindInRoom, [FindMob])) (\ args world -> do
                    item <- asItem $ target1 args
                    targetMob <- asMob $ target2 args
                    updateWorld world [ updateMob (Mob.removeItem item) $ Mob.id $ actor args
                                      , updateMob (Mob.addItem item) $ Mob.id $ targetMob
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "give" "gives", Desc Target1, Const " to ", Desc Target2, Const "."]
                                      ]
                )
                , CommandEntry "say" Nothing Nothing (\ args world ->
                    if xtra args == "" then Left $ "What do you want to say?"
                    else                    sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "say" "says", Sur "'" Xtra] world
                )
                , CommandEntry "inventory" Nothing Nothing (\ args world ->
                    let text = foldl (\acc s -> acc ++ " " ++ s) "Inventory:" (fmap (GameObj.sDesc) (Mob.items $ actor args))
                    in  sendTextMob (actor args) text world
                )
                , CommandEntry "look" Nothing Nothing (\ args world ->
                    do
                        room <- getRoom (locationId $ actor args) world
                        mobs <- getRoomMobs room world
                        elements <- return [ foldl (\acc s -> acc ++ " " ++ s) "Exits:" (fmap GameObj.sDesc $ Room.links room)
                                           , foldl (\acc s -> acc ++ " " ++ s) "Occupants:" (fmap GameObj.sDesc mobs)
                                           , foldl (\acc s -> acc ++ " " ++ s) "Items:" (fmap GameObj.sDesc $ Room.items room)
                                           ]
                        text <- return $ foldl (\acc el -> acc ++ "\n" ++ el) (GameObj.lDesc room) elements
                        sendTextMob (actor args) text world
                )
                , CommandEntry "yell" Nothing Nothing (\ args world ->
                    if xtra args == "" then Left $ "What do you want to yell?"
                    else                    sendMessageTo args MsgGlobal [Desc Actor, Sur " " $ Verb Actor "yell" "yells", Sur "'" Xtra] world
                )
                , CommandEntry "bow" (Just (FindInRoom, findAllTypes)) Nothing (\ args world ->
                    case target1 args of
                        TargetNone -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "bow" "bows", Const "deeply."] world
                        _          -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "bow" "bows", Const "to ", Desc Target1, Const "."] world
                )
                , CommandEntry "clap" (Just (FindInRoom, findAllTypes)) Nothing (\ args world ->
                    case target1 args of
                        TargetNone -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "clap" "claps", Const "enthusiastically."] world
                        _          -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "clap" "claps", Const "at ", Desc Target1, Const "."] world
                )
                , CommandEntry "jump" (Just (FindInRoom, findAllTypes)) Nothing (\ args world ->
                    case target1 args of
                        TargetNone   -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "jump" "jumps", Const "as high as ", Pro Actor, Const " can!" ] world
                        TargetItem _ -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "jump" "jumps", Const "over ", Desc Target1, Const "."] world
                        TargetMob _  -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "get" "gets", Const "ready to jump over ", Desc Target1, Const " but ", Verb Actor "think" "thinks", Const " better of it."] world
                        TargetLink _ -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "jump" "jumps", Const "towards ", Desc Target1, Const " slowly."] world
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
        -- Important: This is the entry point for all input.
        (firstWord, rest) = getFirstWord $ strip line
    in
        -- Do an exact lookup in the map. If that doesn't work, do a startswith lookup against
        -- the list. This lets users abbreviate commands, but also specify a command for an exact
        -- match. When you abbreviate, the order the commands appear in the list above is important.
        case Map.lookup firstWord commandMap of
            Just command -> buildCommand command rest
            Nothing      -> case find (\cmd -> startswith firstWord $ World.name cmd) commandList of
                Just command -> buildCommand command rest
                Nothing -> Invalid line

-- 1. Breaks the first word off a string and converts it to lowercase
-- 2. Strips whitespace both from the word and the remainder
-- Because of thos this routine is used, command tokens and target keywords are ALWAYS lowercase
-- when they are passed to other parts of the system. The rest of the text is passed unmodified
-- except that boundary whitespace of the remained is removed.
getFirstWord :: String -> (String, String)
getFirstWord line = case firstSpace of
    Just index -> (map toLower $ take index line, strip $ drop (index + 1) line)
    Nothing -> (map toLower $ line, [])
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