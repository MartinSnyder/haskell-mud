module Target where

import Data.List (find)

import GameObj
import Item
import Mob
import Link
import Room

data Target = TargetNone
            | TargetItem Item
            | TargetMob Mob
            | TargetLink Link

            
findTargetRoom :: String -> Room -> Target
findTargetRoom keyword room =
    case find (\item -> GameObj.matches item keyword) $ Room.items room of
        Just item -> TargetItem item
        Nothing -> TargetNone

findTargetMob :: String -> Mob -> Target
findTargetMob keyword mob =
    case find (\item -> GameObj.matches item keyword) $ Mob.items mob of
        Just item -> TargetItem item
        Nothing -> TargetNone