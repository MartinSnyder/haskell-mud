module CommandInterpreter (Command, parseCommand, applyCommand) where

import Data.List (elemIndex, take, drop, sort, find)
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.String.Utils (strip, startswith)

import World
import Mob
import Link
import LinkDef
import Room
import Message
import PlayerData
import Target

data Command = Noop
             | Invalid String
             | Go String
             | Look
             | Inventory
             | ExecuteCommand CommandEntry String String String

commandList = [ CommandEntry "help" Nothing Nothing
                    (\ _ _ ->
                        let
                            commands = sort $ Map.keys commandBuilders
                        in
                            Left $ foldl (\acc s -> acc ++ " " ++ s) "Available Commands:" commands
                    )
              , CommandEntry "go" (Just (FindInRoom, [FindLink])) Nothing
                    (\ args world -> do
                        link <- asLink $ target1 args
                        destinationRoomId <- return $ targetRoomId $ Link.def link
                        updateWorld world [ updateRoom (removeMobId $ Mob.id $ actor args) $ roomId $ room args
                                          , updateRoom (addMobId $ Mob.id $ actor args) $ destinationRoomId
                                          , sendMessageTo args MsgRoom [ActorDesc, ActorVerb "leave" "left", Const "the room."]
                                          , sendMessageTo args (MsgSpecificRoom destinationRoomId) [ActorDesc, ActorVerb "enter" "enters", Xtra False, Const "."]
                                          , updateMob (\mob -> Right $ mob { locationId = destinationRoomId }) $ Mob.id $ actor args
                                          ]
              )
              , CommandEntry "get" (Just (FindInRoom, [FindItem])) Nothing
                    (\ args world -> do
                        item <- asItem $ target1 args
                        updateWorld world [ updateRoom (Room.removeItem item) $ roomId $ room args
                                          , updateMob (Mob.addItem item) $ Mob.id $ actor args
                                          , sendMessageTo args MsgRoom [ActorDesc, ActorVerb "take" "takes", Target1Desc, Const "."]
                                          ]
                    )
              , CommandEntry "drop" (Just (FindInActor, [FindItem])) Nothing
                    (\ args world -> do
                        item <- asItem $ target1 args
                        updateWorld world [ updateRoom (Room.addItem item) $ roomId $ room args
                                          , updateMob (Mob.removeItem item) $ Mob.id $ actor args
                                          , sendMessageTo args MsgRoom [ActorDesc, ActorVerb "drop" "drops", Target1Desc, Const "."]
                                          ]
                    )
              , CommandEntry "say" Nothing Nothing
                    (\ args world ->
                        updateWorld world [ sendMessageTo args MsgRoom [ActorDesc, ActorVerb "say" "says", Xtra True] ]
                    )
              , CommandEntry "yell" Nothing Nothing
                    (\ args world ->
                        updateWorld world [ sendMessageTo args MsgGlobal [ActorDesc, ActorVerb "yell" "yells", Xtra True] ]
                    )
              ]

commandMap = Map.fromList $ map (\cmd -> (World.name cmd, cmd)) commandList

commandBuilders = Map.fromList [ ("help", \_ -> parseCommand2 $ "help")
                               , ("yell", \rest -> parseCommand2 $ "yell " ++ rest)
                               , ("say", \rest -> parseCommand2 $ "say " ++ rest)
                               , ("go", \rest -> parseCommand2 $ "go " ++ rest)
                               , ("look", \_ -> Look)
                               , ("inventory", \_ -> Inventory)
                               , ("get", \rest -> parseCommand2 $ "get " ++ rest)
                               , ("drop", \rest -> parseCommand2 $ "drop " ++ rest)
                               ]

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

parseCommand2 :: String -> Command
parseCommand2 "" = Noop
parseCommand2 line =
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

parseCommand :: String -> Command
parseCommand "" = Noop
parseCommand line =
    let
        -- Important: This is the entry point for all input. Before even looking at it, we:
        -- 1. Convert to lowercase AND
        -- 2. Strip boundary whitespace
        (firstWord, rest) = getFirstWord . strip $ map toLower line
    in case Map.lookup firstWord commandBuilders of
        Just builder -> builder $ rest
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
        Look ->
            lookRoom userId world
        Inventory ->
            showInventory userId world
        ExecuteCommand cmdEntry keyword1 keyword2 extra ->
            doCommand userId cmdEntry keyword1 keyword2 extra world