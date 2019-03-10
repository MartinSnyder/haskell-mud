module Main where

import System.Environment

import World (buildWorld)
import ProcLibrary (roomProcedureLibrary)
import Repl
import WebServer

main = do
    args <- getArgs
    fileContents <- readFile "world.txt"
    worldDef <- return $ read fileContents
    world <- return $ buildWorld worldDef roomProcedureLibrary
    case args of
        [] -> do
            port <- read <$> getEnv "PORT"
            webServer world port
        ["console"] ->
            repl world
        [port] -> do
            webServer world $ read port
        _ ->
            error "Too many command line arguments specified"