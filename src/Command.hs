module Command ( Command
               , parseCommand
               , applyCommand
               ) where

import Data.List (elemIndex, take, drop, sort)
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.String.Utils (strip)

import World
import Message
import PlayerData

data Command = Noop
             | Invalid String
             | Help
             | Broadcast String
             | Yell String
             | Say String
             | Go String
             | Look
             | Inventory
             | Get String
             deriving (Eq, Show)

commandBuilders = Map.fromList [ ("help", \_ -> Help)
                               , ("broadcast", \rest -> Broadcast rest)
                               , ("yell", \rest -> Yell rest)
                               , ("say", \rest -> Say rest)
                               , ("go", \rest -> Go rest)
                               , ("look", \_ -> Look)
                               , ("inventory", \_ -> Inventory)
                               , ("get", \rest -> Get rest)
                               ]

parseCommand :: String -> Command
parseCommand "" = Noop
parseCommand line =
    let
        -- Important: This is the entry point for all input. Before even looking at it, we:
        -- 1. Convert to lowercase AND
        -- 2. Strip boundary whitespace
        (firstWord, rest) = getFirstWord . strip $ map toLower line
    in case Map.lookup firstWord commandBuilders of
        Just builder -> builder $ strip rest -- Additional strip here in case there is extra whitespace after the first word
        Nothing -> Invalid line

getFirstWord :: String -> (String, String)
getFirstWord line = case firstSpace of
    Just index -> (take index line, drop (index + 1) line)
    Nothing -> (line, [])
    where firstSpace = elemIndex ' ' line


applyCommand :: UserId -> Command -> World -> Either String World
applyCommand userId command world =
    case command of
        Noop ->
            Right world
        Invalid text ->
            Left $ "Invalid input: " ++ text
        Help ->
            let
                commands = sort $ Map.keys commandBuilders
            in
                Left $ foldl (\acc s -> acc ++ " " ++ s) "Available Commands:" commands
        Broadcast message ->
            sendBroadcastMessage message world
        Yell text ->
            sendGlobalMessage userId Nothing text message world
            where message = [Actor, ActorVerb "yell" "yells", Xtra True]
        Say text ->
            sendLocalMessage userId Nothing text message world
            where message = [Actor, ActorVerb "say" "says", Xtra True]
        Go dir ->
            followLink userId dir world
        Look ->
            lookRoom userId world
        Inventory ->
            showInventory userId world
        Get item ->
            if (item == "all") then
                getItem userId Nothing world
            else
                getItem userId (Just item) world