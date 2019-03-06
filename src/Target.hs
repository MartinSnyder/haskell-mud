module Target( Target(..)
             , FindIn(..)
             , FindType(..)
             , findAllTypes
             , findTarget
             ) where

import Data.List (find)
import Data.Map (elems)

import GameObj
import Item
import Mob
import Link
import Room

data Target = TargetNone
            | TargetItem Item
            | TargetMob Mob
            | TargetLink Link
            deriving (Eq)

data FindIn = FindInRoom | FindInActor | FindNowhere

data FindType = FindItem | FindMob | FindLink

findAllTypes = [ FindItem, FindMob, FindLink ]

findTarget :: FindIn -> [FindType] -> String -> Mob -> Room -> [Mob] -> Target
findTarget findIn findTypes keyword actor room roomMobs =
    foldl (\ acc findType ->
            if acc == TargetNone then
                findTargetSingleType findIn findType keyword actor room roomMobs
            else
                acc
          ) TargetNone findTypes

findInList :: GameObj a => String -> (a -> Target) -> [a] -> Target
findInList keyword ctor list =
    case find (\obj -> GameObj.matches obj keyword) $ list of
        Just obj -> ctor obj
        Nothing -> TargetNone

findTargetSingleType :: FindIn -> FindType -> String -> Mob -> Room -> [Mob] -> Target
findTargetSingleType findIn findType keyword actor room roomMobs =
    case findIn of
        FindInRoom -> case findType of
            FindItem -> findInList keyword TargetItem $ Room.items room
            FindMob -> findInList keyword TargetMob roomMobs
            FindLink -> findInList keyword TargetLink $ elems $ Room.links room
        FindInActor -> case findType of
            FindItem -> findInList keyword TargetItem $ Mob.items actor
            _ -> TargetNone
        FindNowhere -> TargetNone