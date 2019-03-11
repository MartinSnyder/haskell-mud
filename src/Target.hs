module Target( Target(..)
             , FindIn(..)
             , FindType(..)
             , findAllTypes
             , findTarget
             , asItem
             , asMob
             , asLink
             ) where

import Data.List (find)
import Data.Map (elems)
import Data.Maybe (isNothing, fromMaybe)

import GameObj
import Item
import Mob
import Link
import Room

data Target = TargetNone
            | TargetItem Item
            | TargetMob Mob
            | TargetLink Link

data FindIn = FindInRoom | FindInActor

data FindType = FindItem | FindMob | FindLink

findAllTypes = [ FindItem, FindMob, FindLink ]

findTarget :: FindIn -> [FindType] -> String -> Mob -> Room -> [Mob] -> Target
findTarget findIn findTypes keyword actor room roomMobs =
    if keyword == "" then
        TargetNone
    else
        fromMaybe TargetNone $ foldl (\ acc findType -> if isNothing acc then
                                                            findTargetSingleType findIn findType keyword actor room roomMobs
                                                        else
                                                            acc
                                     ) Nothing findTypes

findInList :: GameObj a => String -> (a -> Target) -> [a] -> Maybe Target
findInList keyword ctor list =
    fmap ctor $ find (\obj -> GameObj.matches obj keyword) $ list

findTargetSingleType :: FindIn -> FindType -> String -> Mob -> Room -> [Mob] -> Maybe Target
findTargetSingleType findIn findType keyword actor room roomMobs =
    case findIn of
        FindInRoom -> case findType of
            FindItem -> findInList keyword TargetItem $ Room.items room
            FindMob -> findInList keyword TargetMob roomMobs
            FindLink -> findInList keyword TargetLink $ Room.links room
        FindInActor -> case findType of
            FindItem -> findInList keyword TargetItem $ Mob.items actor
            _ -> Nothing

asItem :: Target -> Either String Item
asItem target = case target of
    TargetItem item -> Right item
    TargetNone -> Left "No item specified"
    _ -> Left "That is not an item"

asMob :: Target -> Either String Mob
asMob target = case target of
    TargetMob mob -> Right mob
    TargetNone -> Left "No creature specified"
    _ -> Left "That is not a creature"

asLink :: Target -> Either String Link
asLink target = case target of
    TargetLink link -> Right link
    TargetNone -> Left "No exit specified"
    _ -> Left "That is not a exit"