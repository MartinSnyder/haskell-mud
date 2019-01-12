module Repl ( repl) where

import Control.Monad (unless)
import System.IO

import RoomDef
import Link
import World (World, buildWorld, addPlayerIfAbsent, extractOutput)
import Connection
import PlayerData
import Room
import Command

data Directive = Quit | Login UserId

repl :: World -> IO ()
repl world = do
    _ <- putStr "Enter your UserId: "
    userId <- getLine
    world' <- printOutput userId world $ addPlayerIfAbsent userId world
    gameLoop world' userId

gameLoop :: World -> UserId -> IO()
gameLoop world userId = do
  input <- readInput world userId
  case input of
    Left Quit -> return ()
    Left (Login nextUserId) -> do
        world' <- printOutput nextUserId world $ addPlayerIfAbsent nextUserId world
        gameLoop world' nextUserId
    Right command -> do
        postOutputWorld <- printOutput userId world $ applyCommand userId command world
        gameLoop postOutputWorld userId

readInput :: World -> UserId -> IO (Either Directive Command)
readInput world userId = do
    _ <- putStr $ "[" ++ userId ++ "]> "
    _ <- hFlush stdout
    input <- getLine
    return $ case input of
        ":quit" -> Left Quit
        'l' : 'o' : 'g' : 'i' : 'n' : ' ' : userId -> Left $ Login userId
        other -> Right $ parseCommand input

printOutput :: UserId -> World -> Either String World -> IO (World)
printOutput userId prevWorld result =
  let
    (nextWorld, messages) = case result of
        Left err ->
            (prevWorld, [err])
        Right resultWorld ->
            World.extractOutput userId resultWorld
    output = foldl (\acc s -> acc ++ s ++ "\n") "" messages
  in do
    putStrLn output >> return nextWorld