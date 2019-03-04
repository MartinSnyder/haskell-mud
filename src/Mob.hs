module Mob where

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
        Right pd -> keyword == PlayerData.name pd