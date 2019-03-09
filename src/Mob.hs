module Mob where

import Data.List (partition)
import Data.Char (toLower)

import GameDef
import GameObj
import MobDef
import PlayerData
import Item

type MobId = Integer

data Mob = Mob { id :: MobId
               , locationId :: DefId
               , items :: [Item]
               , connectionId :: Maybe UserId
               , base :: Either MobDef PlayerData
               } deriving (Show, Eq)

instance GameObj Mob where
    sDesc mob = case Mob.base mob of
        Left def -> GameDef.sDesc def
        Right pd -> PlayerData.name pd
    lDesc mob = case Mob.base mob of
        Left def -> GameDef.lDesc def
        Right pd -> "A player named " ++ PlayerData.name pd
    matches mob keyword = case Mob.base mob of
        Left def -> GameDef.matches def keyword
        Right pd -> keyword == (map toLower $ PlayerData.name pd)

addItem :: Item -> Mob -> Either String Mob
addItem item mob =
    Right $ mob { Mob.items = item : (Mob.items mob) }

removeItem :: Item -> Mob -> Either String Mob
removeItem item mob =
    case partition (== item) (Mob.items mob) of
        ([], _) -> Left $ "Item " ++ (GameObj.sDesc item) ++ " not found on mob " ++ show (Mob.id mob)
        ([ foundItem ], remainingItems) -> Right $ mob { Mob.items = remainingItems }
