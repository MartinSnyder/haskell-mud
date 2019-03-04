module LinkDef where

import GameDef

data LinkDef = LinkDef { name :: String
                       , targetRoomId :: DefId
                       } deriving (Show, Eq)