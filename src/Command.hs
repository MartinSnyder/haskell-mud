module Command ( Command
               , parseCommand
               , applyCommand
               ) where

import Data.List (elemIndex, take, drop)
import qualified Data.Map as Map

import World
import Message
import PlayerData

data Command = Noop
             | Invalid String
             | Broadcast String
             | Yell String
             | Say String
             | Go String
             | Look
             deriving (Eq, Show)

commandBuilders = Map.fromList [ ("broadcast", \rest -> Broadcast rest)
                               , ("yell", \rest -> Yell rest)
                               , ("say", \rest -> Say rest)
                               , ("go", \rest -> Go rest)
                               , ("look", \_ -> Look)
                               ]

getFirstWord :: String -> (String, String)
getFirstWord line = case firstSpace of
    Just index -> (take index line, drop (index + 1) line)
    Nothing -> (line, [])
    where firstSpace = elemIndex ' ' line

parseCommand :: String -> Command
parseCommand "" = Noop
parseCommand line =
    let
        (firstWord, rest) = getFirstWord line
    in case Map.lookup firstWord commandBuilders of
        Just builder -> builder rest
        Nothing -> Invalid line

applyCommand :: UserId -> Command -> World -> Either String World
applyCommand userId command world =
    case command of
        Noop ->
            Right world
        Invalid text ->
            Left $ "Invalid input: " ++ text
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