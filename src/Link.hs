module Link where

import GameDef

data Link = Link { targetRoomId :: DefId
                 } deriving (Show, Eq)