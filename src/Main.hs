module Main where

import qualified Data.Map as Map
import System.Environment

import RoomDef
import ItemDef
import Link
import World (buildWorld)
import Repl
import WebServer

initialWorld = buildWorld (0, 0)
  [ RoomDef
        (0, 0)
        "The Spawning Pool"
        "Creatures of varying size form within the primordial soup!"
        [ ItemDef (0, 0) "A flat stone" "Time has smoothed this stone to have a perfectly flat surface" ]
        $ Map.fromList [ ("Down", Link (1,0)) ]
  , RoomDef
        (1, 0)
        "Alpha Hill"
        "A quaint grassy hill. A dirt path to the south leads to a coastal town"
        []
        $ Map.fromList [ ("South", Link (1,1)) ]
  , RoomDef
        (1, 1)
        "Dusty path"
        "The dirt of the path is beaten flat and firm, but the dust still manages to cover your shoes."
        []
        $ Map.fromList [ ("North", Link (1,0)) ]
  ]

main = do
    args <- getArgs
    case args of
        [] -> do
            port <- read <$> getEnv "PORT"
            webServer initialWorld port
        ["console"] ->
            repl initialWorld
        [port] -> do
            webServer initialWorld $ read port
        _ ->
            error "Too many command line arguments specified"