module Connection where

import Data.List as List
import Room
import PlayerData
import Mob

data Connection = Connection { userId :: UserId
                             , mobId :: MobId
                             , output :: [String]
                             } deriving (Show, Eq)

sendText :: String -> Connection -> Connection
sendText text conn =
    conn { output = text : output conn }