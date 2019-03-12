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
import Passage
import RoomDef
import Message
import PlayerData
import Target
import CommandHelpers

data Command = Noop
             | Invalid String
             | ExecuteCommand CommandEntry String String String

commandList =   [ CommandEntry "help" targetNothing targetNothing (\ _ _ ->
                    let commands = sort $ map World.name commandList
                    in  Left $ foldl (\acc s -> acc ++ " " ++ s) "Available Commands:" commands
                )
                , CommandEntry "go" [FindRoomLink] targetNothing (\ args world -> do
                    room <- getRoom (locationId $ actor args) world
                    link <- asLink $ target1 args
                    passageIsClosed <- testPassage link world isClosed
                    _ <- if passageIsClosed then Left $ (GameObj.sDesc link) ++ " is closed." else Right ()
                    destinationRoomId <- return $ targetRoomId $ Link.def link
                    destinationRoom <- getRoom destinationRoomId world
                    destinationRoomMobs <- getRoomMobs destinationRoom world
                    updateWorld world [ updateRoom (removeMobId $ Mob.id $ actor args) $ locationId $ actor args
                                      , updateRoom (addMobId $ Mob.id $ actor args) destinationRoomId
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "leave" "left", Const "the room via ", Desc Target1, Const "."]
                                      , sendMessageTo args (MsgRoom destinationRoomId) [Desc Actor, Sur " " $ Verb Actor "arrive" "arrives", Const $ "from " ++ (GameObj.sDesc room), Const "."]
                                      , updateMob (\mob -> Right $ mob { locationId = destinationRoomId }) $ Mob.id $ actor args
                                      , sendTextMob (actor args) $ lookRoomShort destinationRoom destinationRoomMobs
                                      ]
                )
                , CommandEntry "open" [FindRoomLink] targetNothing (\ args world -> do
                    room <- getRoom (locationId $ actor args) world
                    link <- asLink $ target1 args
                    _ <- if not $ canClose link then Left $ (GameObj.sDesc link) ++ " does not open or close." else Right ()
                    updateWorld world [ updateLink (\p -> if isClosed p then Right $ p { isClosed = False }
                                                                        else Left $ (GameObj.sDesc p) ++ " is already open.") link
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "open" "opens", Desc Target1, Const "."]
                                      ]
                )
                , CommandEntry "close" [FindRoomLink] targetNothing (\ args world -> do
                    room <- getRoom (locationId $ actor args) world
                    link <- asLink $ target1 args
                    _ <- if not $ canClose link then Left $ (GameObj.sDesc link) ++ " does not open or close." else Right ()
                    updateWorld world [ updateLink (\p -> if isClosed p then Left $ (GameObj.sDesc p) ++ " is already closed."
                                                                        else Right $ p { isClosed = True }) link
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "close" "closes", Desc Target1, Const "."]
                                      ]
                )
                , CommandEntry "get" [FindRoomItem] targetNothing (\ args world -> do
                    item <- asItem $ target1 args
                    updateWorld world [ updateRoom (Room.removeItem item) $ locationId $ actor args
                                      , updateMob (Mob.addItem item) $ Mob.id $ actor args
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "take" "takes", Desc Target1, Const "."]
                                      ]
                )
                , CommandEntry "drop" [FindActorItem] targetNothing (\ args world -> do
                    item <- asItem $ target1 args
                    updateWorld world [ updateRoom (Room.addItem item) $ locationId $ actor args
                                      , updateMob (Mob.removeItem item) $ Mob.id $ actor args
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "drop" "drops", Desc Target1, Const "."]
                                      ]
                )
                , CommandEntry "give" [FindActorItem] [FindRoomMob] (\ args world -> do
                    item <- asItem $ target1 args
                    targetMob <- asMob $ target2 args
                    updateWorld world [ updateMob (Mob.removeItem item) $ Mob.id $ actor args
                                      , updateMob (Mob.addItem item) $ Mob.id $ targetMob
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "give" "gives", Desc Target1, Const " to ", Desc Target2, Const "."]
                                      ]
                )
                , CommandEntry "say" targetNothing targetNothing (\ args world ->
                    if xtra args == "" then Left $ "What do you want to say?"
                    else                    sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "say" "says", Sur "'" Xtra] world
                )
                , CommandEntry "inventory" targetNothing targetNothing (\ args world ->
                    let text = foldl (\acc s -> acc ++ " " ++ s) "Inventory:" (fmap (GameObj.sDesc) (Mob.items $ actor args))
                    in  sendTextMob (actor args) text world
                )
                , CommandEntry "look" targetAnything targetNothing (\ args world ->
                    case target1 args of
                        TargetNone -> do
                            room <- getRoom (locationId $ actor args) world
                            mobs <- getRoomMobs room world
                            sendTextMob (actor args) (lookRoomLong room mobs) world
                        TargetItem item -> sendTextMob (actor args) (GameObj.lDesc item) world
                        TargetMob mob   -> sendTextMob (actor args) (GameObj.lDesc mob) world
                        TargetLink link -> sendTextMob (actor args) (GameObj.lDesc link) world
                )
                , CommandEntry "exits" targetNothing targetNothing (\ args world ->
                    do
                        room <- getRoom (locationId $ actor args) world
                        sendTextMob (actor args) (formatContents "There are no available exits." "Exits: " exitString $ Room.links room) world
                )
                , CommandEntry "yell" targetNothing targetNothing (\ args world ->
                    if xtra args == "" then Left $ "What do you want to yell?"
                    else                    sendMessageTo args MsgGlobal [Desc Actor, Sur " " $ Verb Actor "yell" "yells", Sur "'" Xtra] world
                )
                , CommandEntry "bow" targetAnythingInRoom targetNothing (\ args world ->
                    case target1 args of
                        TargetNone -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "bow" "bows", Const "deeply."] world
                        _          -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "bow" "bows", Const "to ", Desc Target1, Const "."] world
                )
                , CommandEntry "clap" targetAnythingInRoom targetNothing (\ args world ->
                    case target1 args of
                        TargetNone -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "clap" "claps", Const "enthusiastically."] world
                        _          -> sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "clap" "claps", Const "at ", Desc Target1, Const "."] world
                )
                , CommandEntry "jump" targetAnythingInRoom targetNothing (\ args world ->
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
            [] -> ("", rest)
            _ -> getFirstWord rest

        (keyword2, rest'') = case target2Spec command of
            [] -> ("", rest')
            _ -> getFirstWord rest'
    in
        ExecuteCommand command keyword1 keyword2 rest''

replaceShortcuts :: String -> String
replaceShortcuts line = 
    case strip line of
        '\'' : rest -> "say " ++ rest          -- preserve original case in this circumstance
        _ -> case strip $ fmap toLower line of -- case insensitive comparisons
            "north" -> "go north"
            "south" -> "go south"
            "east"  -> "go east"
            "west"  -> "go west"
            "up"    -> "go up"
            "down"  -> "go down"
            _ -> line -- Unmodified

parseCommand :: String -> Command
parseCommand "" = Noop
parseCommand line =
    let
        -- Important: This is the entry point for all input.
        (firstWord, rest) = getFirstWord $ strip $ replaceShortcuts line
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