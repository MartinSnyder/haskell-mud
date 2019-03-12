module ProcLibrary (roomProcedureLibrary) where

import Data.Map

import GameObj
import Mob
import Item
import Room
import World
import Message
import Target
import CommandHelpers

roomProcedureLibrary :: Map String RoomProcedure
roomProcedureLibrary = fromList([
        ("unstableFloor", (\ command args world -> case name command of
            "jump" -> sendMessageTo args MsgActorRoom [Const "The floor shifts and tilts when ", Desc Actor, Sp, Verb Actor "jump" "jumps", Const " on it."] world
            _      -> Right world
        ))
      , ("lookOutBelow", (\ command args world -> case (name command, target1 args) of
            ("jump", TargetItem item) ->
                if (itemId item == (0,1)) then do
                    room <- getRoom (locationId $ actor args) world
                    destinationRoomId <- return (1,5)
                    destinationRoom <- getRoom destinationRoomId world
                    destinationRoomMobs <- getRoomMobs destinationRoom world
                    updateWorld world [ updateRoom (removeMobId $ Mob.id $ actor args) $ locationId $ actor args
                                      , updateRoom (addMobId $ Mob.id $ actor args) destinationRoomId
                                      , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "fly" "flies", Const "over ", Desc Target1, Const " to the courtyard below."]
                                      , sendMessageTo args (MsgRoom destinationRoomId) [Desc Actor, Sur " " $ Verb Actor "crash" "crashes", Const $ "down from above!"]
                                      , sendMessageTo args (MsgRoom destinationRoomId) [Const "The floor buckles violently when ", Desc Actor, Sp, Verb Actor "fall" "falls", Const " on it."]
                                      , updateMob (\mob -> Right $ mob { locationId = destinationRoomId }) $ Mob.id $ actor args
                                      , sendTextMob (actor args) $ lookRoomShort destinationRoom destinationRoomMobs
                                      ]
                else
                    Right world
            _ -> Right world
      ))
    ])