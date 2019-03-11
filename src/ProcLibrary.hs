module ProcLibrary (roomProcedureLibrary) where

import Data.Map

import World
import Message

roomProcedureLibrary :: Map String RoomProcedure
roomProcedureLibrary = fromList([
        ("unstableFloor", (\ command args world ->
            if name command == "jump" then
                sendMessageTo args MsgActorRoom [Const "The floor shifts and tilts when ", Desc Actor, Sp, Verb Actor "jump" "jumps", Const " on it."] world
            else
                Right world
        ))
    ])