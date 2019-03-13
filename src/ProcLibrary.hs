module ProcLibrary (roomProcedureLibrary) where

import Data.Map (Map, fromList)

import GameObj
import Mob
import Item
import Room
import World
import Message
import Target
import CommandHelpers

marbleRailingId = (0,1)
westCourtyardId = (1,5)
eastCourtyardId = (1,6)
eastGalleryId = (1,8)

roomProcedureLibrary :: Map String RoomProcedure
roomProcedureLibrary = fromList([
        ("unstableFloor", (\ command args world -> case name command of
            "jump" -> sendMessageTo args MsgActorRoom [Const "The floor shifts and tilts when ", Desc Actor, Sp, Verb Actor "jump" "jumps", Const " on it."] world
            _      -> Right world
        ))
      , ("lookOutBelow", (\ command args world -> case (name command, target1 args) of
            ("jump", TargetItem item) ->
                if (itemId item == marbleRailingId) then do
                    room <- getRoom (locationId $ actor args) world
                    westCourtyardRoom <- getRoom westCourtyardId world
                    westCourtyardMobs <- getRoomMobs westCourtyardRoom world
                    eastCourtyardRoom <- getRoom eastCourtyardId world
                    mobsIdsGettingThrown  <- Right $ mobIds eastCourtyardRoom
                    eastGalleryRoom <- getRoom eastGalleryId world
                    world' <- updateWorld world [ updateRoom (removeMobId $ Mob.id $ actor args) $ locationId $ actor args
                                                , updateRoom (addMobId $ Mob.id $ actor args) westCourtyardId
                                                , sendMessageTo args MsgActorRoom [Desc Actor, Sur " " $ Verb Actor "fly" "flies", Const "over ", Desc Target1, Const " to the courtyard below."]
                                                , sendMessageTo args (MsgRoom westCourtyardId) [Desc Actor, Sur " " $ Verb Actor "crash" "crashes", Const $ "down from above!"]
                                                , sendMessageTo args (MsgRoom westCourtyardId) [Const "The floor buckles violently when ", Desc Actor, Sp, Verb Actor "fall" "falls", Const " on it."]
                                                , updateMob (\mob -> Right $ mob { locationId = westCourtyardId }) $ Mob.id $ actor args
                                                , sendTextMob (actor args) $ lookRoomShort westCourtyardRoom westCourtyardMobs
                                                , sendMessageTo args (MsgRoom eastCourtyardId) [Const "The floor buckles violently and throws everyone to the gallery above!"]
                                                , sendMessageTo args (MsgRoom eastGalleryId) [Const "There is a crash below and bodies fly into the gallery"]
                                                , updateRoom (\room -> Right $ room { mobIds = (mobIds eastCourtyardRoom) ++ (mobIds eastGalleryRoom) }) eastGalleryId
                                                , updateRoom (\room -> Right $ room { mobIds = [] }) eastCourtyardId
                                                ]
                    foldl (\ acc mobId -> acc >>= (\accWorld -> updateMob (\mob -> Right $ mob { locationId = eastGalleryId }) mobId accWorld)) (Right world') mobsIdsGettingThrown
                else
                    Right world
            _ -> Right world
      ))
    ])