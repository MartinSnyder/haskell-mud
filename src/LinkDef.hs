module LinkDef where

import Data.Char (toLower)

import GameDef
import GameObj

data Direction = North | South | East | West | Up | Down | Enter String deriving (Show, Read)

data LinkDef = LinkDef { direction :: Direction
                       , targetRoomId :: DefId
                       } deriving (Show, Read)

getKeyword :: Direction -> String
getKeyword North = "North"
getKeyword South = "South"
getKeyword East  = "East"
getKeyword West  = "West"
getKeyword Up    = "Up"
getKeyword Down  = "Down"
getKeyword (Enter key) = key

instance GameDef LinkDef where
    defId def = (-1, -1)
    sDesc def = getKeyword $ LinkDef.direction def
    lDesc def = getKeyword $ LinkDef.direction def
    matches def keyword = keyword == (map toLower $ getKeyword $ direction def)