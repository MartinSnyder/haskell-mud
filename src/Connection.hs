module Connection where

import Data.List as List
import Room
import PlayerData
import Mob

data Connection = Connection { userId :: UserId
                             , mobId :: MobId
                             , messages :: [String]
                             } deriving (Show, Eq)

sendMessage :: String -> Connection -> Connection
sendMessage message conn =
    conn { messages = message : Connection.messages conn }