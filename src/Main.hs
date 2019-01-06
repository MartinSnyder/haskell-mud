module Main where

import qualified Data.Map as Map

import RoomDef
import Link
import World (buildWorld)
import Repl

initialWorld = buildWorld (0, 0)
  [ RoomDef (0, 0) "The Spawning Pool" "Creatures of varying size form within the primordial soup!"
      $ Map.fromList [ ("Down", Link (1,0)) ]
  , RoomDef (1, 0) "Alpha Hill" "A quaint grassy hill. A dirt path to the south leads to a coastal town"
      $ Map.fromList [ ("South", Link (1,1)) ]
  , RoomDef (1, 1) "Dusty path" "The dirt of the path is beaten flat and firm, but the dust still manages to cover your shoes."
      $ Map.fromList [ ("North", Link (1,0)) ]
  ]

main = repl initialWorld